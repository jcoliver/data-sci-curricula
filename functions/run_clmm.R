#' Run ordinal mixed-effect model on area scores
#' 
#' @param framework  character vector indicating which framework to analyze
run_clmm <- function(framework = c("na", "gds")) {
  if(!require(tidyr)) { # Data wrangling, especially pivot_longer
    stop("run_clmm requires tidyr package, which could not be loaded.")
  }
  if(!require(magrittr)) { # The pipe %>% operator
    stop("run_clmm requires dplyr package, which could not be loaded.")
  }
  if(!require(ordinal)){ # Ordinal mixed effects regression
    stop("run_clmm requires ordinal package, which could not be loaded.")
  }
  if(!require(emmeans)) { # Post-hoc pairwise comparisons
    stop("run_clmm requires emmeans package, which could not be loaded.")
  }
  
  score_file <- paste0("data/", framework, "-scores.csv")
  scores <- read.csv(file = score_file, stringsAsFactors = FALSE)
  areas_file <- paste0("data/", framework, "-areas-key.csv")
  areas <- read.csv(file = areas_file, stringsAsFactors = FALSE)
  
  # Convert scores to long
  # Skip Row.Type, Institution, and Program
  # During pivot, drop data from columns with higher area name, as they have no 
  # data
  area_prefix <- paste0(toupper(x = framework), ".")
  scores_long <- scores %>%
    pivot_longer(cols = starts_with(match = area_prefix),
                 names_to = "Subarea", 
                 values_to = "Score",
                 values_drop_na = TRUE)
  
  # Need to assign Area for each subarea.
  scores_long <- merge(x = scores_long, 
                       y = areas[, c("Area", "Key")],
                       by.x = "Subarea",
                       by.y = "Key")
  
  # Explicitly cast Area as a factor for subsequent assumption tests
  scores_long$Area <- factor(x = scores_long$Area)
  
  # Ordinal package is expecting a factor for the response variable
  scores_long$ScoreOrdinal <- as.factor(scores_long$Score)

  # Run mixed model: Area as fixed, program as random; 
  # setting threshold = "equidistant" avoids downstream errors in post-hoc tests
  scores_clmm <- ordinal::clmm(ScoreOrdinal ~ Area + (1|Program),
                               data = scores_long,
                               threshold = "equidistant") 
  
  
  # Write model results to file
  output_file <- paste0("output/", framework, "-clmm.txt")
  sink(file = output_file)
  cat("Linear mixed effects model results\n\n")
  print(summary(scores_clmm))
  sink()  
  
  # Run post-hoc pairwise comparisons
  scores_posthoc <- emmeans(scores_clmm,
                            pairwise ~ Area,
                            adjust = "Tukey")
  
  # Extract pairwise comparison p-values
  scores_posthoc_df <- as.data.frame(scores_posthoc$contrasts)
  
  # Create Area x Area matrix to place p-values in lower diagonal, z-scores in 
  # upper diagonal
  area_names <- levels(scores_long$Area)
  scores_posthoc_mat <- matrix(data = NA, 
                               nrow = length(area_names), 
                               ncol = length(area_names))
  
  rownames(scores_posthoc_mat) <- area_names
  colnames(scores_posthoc_mat) <- area_names
  
  for (a_r in 1:length(area_names)) {
    for (a_c in 1:length(area_names)) {
      if (a_r < a_c) {
        # Which two areas are being compared?
        contrast_i <- paste0(area_names[a_r], " - ", area_names[a_c])
        # Which row of the contrast data frame is this comparison in?
        df_row <- which(scores_posthoc_df$contrast == contrast_i)
        # Assign p-value to appropriate matrix cell (above diagonal)
        p_value <- round(scores_posthoc_df$p.value[df_row], digits = 4)
        scores_posthoc_mat[a_r, a_c] <- p_value
        # Assign z-ratio to appropriate matrix cell (below diagonal)
        z_score <- round(scores_posthoc_df$z.ratio[df_row], digits = 3)
        scores_posthoc_mat[a_c, a_r] <- z_score
      }
    }
  }
  
  # Need to replace the p = 0 with character strings < 0.0001
  scores_posthoc_char <- matrix(data = as.character(scores_posthoc_mat),
                                nrow = nrow(scores_posthoc_mat),
                                ncol = ncol(scores_posthoc_mat))
  scores_posthoc_char[scores_posthoc_mat == 0] <- "< 0.0001"
  rownames(scores_posthoc_char) <- rownames(scores_posthoc_mat)
  colnames(scores_posthoc_char) <- colnames(scores_posthoc_mat)
  
  write.csv(x = scores_posthoc_char, 
            file = paste0("output/", framework, "-clmm-posthoc.csv"),
            row.names = TRUE)
}
