#' Run ordinal mixed-effect model on area scores
#' 
#' @param framework  character vector indicating which framework to analyze
#' 
#' @return a list with three elements
#' \itemize{
#'   \item{"framework"}{the framework that was analyzed}
#'   \item{"clmm_model"}{the model object output from \code{ordinal::clmm}}
#'   \item{"posthoc"}{result of post-hoc comparisons from a call to 
#'   \code{emmeans::emmeans}}
#' }
run_clmm <- function(framework = c("nasem", "gds")) {
  if(!require(tidyr)) { # Data wrangling, pivot_longer
    stop("run_clmm requires tidyr package, which could not be loaded.")
  }
  if(!require(dplyr)) { # Data wrangling, select and the pipe %>% operator
    stop("run_clmm requires dplyr package, which could not be loaded.")
  }
  if(!require(ordinal)){ # Ordinal mixed effects regression
    stop("run_clmm requires ordinal package, which could not be loaded.")
  }
  if(!require(emmeans)) { # Post-hoc pairwise comparisons
    stop("run_clmm requires emmeans package, which could not be loaded.")
  }
  
  score_file <- paste0("data/scores-", framework, ".csv")
  scores <- read.csv(file = score_file, stringsAsFactors = FALSE)

  areas_file <- paste0("data/areas-key-", framework, ".csv")
  areas <- read.csv(file = areas_file, stringsAsFactors = FALSE)
  
  # Convert scores to long
  # Skip Institution
  # During pivot, drop data from columns with higher area name, as they have no 
  # data
  area_prefix <- paste0(toupper(x = framework), ".")
  scores_long <- scores %>%
    pivot_longer(cols = starts_with(match = area_prefix),
                 names_to = "Subarea", 
                 values_to = "Score",
                 values_drop_na = TRUE) %>%
    select(-Institution)
  
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
  
  if (!dir.exists(paths = "output")) {
    dir.create("output")
  }

  # Write model results to file
  output_file <- paste0("output/", framework, "-clmm.txt")
  sink(file = output_file)
  cat("Linear mixed effects model results\n\n")
  print(summary(scores_clmm))
  # In attempt to get omnibus test statistics, there does not seem to be 
  # an anova.clmm, but see https://rdrr.io/cran/RVAideMemoire/src/R/Anova.clmm.R
  # for a possible way of manually calculating this stuff
  # print(anova(scores_clmm))
  sink()  
  
  # Run post-hoc pairwise comparisons
  scores_posthoc <- emmeans::emmeans(scores_clmm,
                                     pairwise ~ Area,
                                     adjust = "Tukey")
  
  # Extract pairwise comparison p-values
  scores_posthoc_df <- as.data.frame(scores_posthoc$contrasts)
  
  # Create Area x Area matrix to place p-values in lower diagonal, z-scores in 
  # upper diagonal
  # area_names <- levels(scores_long$Area)

  # We'll want to order these by highest scoring areas first, lowest scoring 
  # areas last, so calculate means and use that to extract order for matrix
  options(dplyr.summarise.inform = FALSE)  # Turn off ungrouping message
  area_means <- scores_long %>%
    group_by(Area) %>%
    summarize(area_mean = mean(Score)) %>%
    arrange(desc(area_mean))
  options(dplyr.summarise.inform = TRUE) # Revert to default messaging behavior
  
  area_names <- area_means$Area
  
  scores_posthoc_mat <- matrix(data = NA, 
                               nrow = length(area_names), 
                               ncol = length(area_names))
  
  rownames(scores_posthoc_mat) <- area_names
  colnames(scores_posthoc_mat) <- area_names
  
  for (a_r in 1:length(area_names)) {
    for (a_c in 1:length(area_names)) {
      if (a_r < a_c) {
        # In the post-hoc output of emmeans, the contrast is labeled as 
        # "Group i - Group j", where i is always a lower level than j. 
        # Generally, this means that contrasts are presented in alphabetical 
        # order. i.e. the contrast between Communication and Computing should 
        # be presented as "Communication - Computing", _not_ as 
        # "Computing - Communication". Because we are ordering the matrix rows/
        # columns by the mean score of the area and not by alphabetical order, 
        # will need to be creative and check for a non existent comparison as 
        # we cycle over the contrasts

        # Which two areas are being compared?
        contrast_i <- paste0(area_names[a_r], " - ", area_names[a_c])
        # Which row of the contrast data frame is this comparison in?
        df_row <- which(scores_posthoc_df$contrast == contrast_i)

        # Here we check to make sure we were able to pull out the value of the 
        # contrast; if not, reverse the order of area names to line up with 
        # the output from emmeans
        if (length(df_row) < 1) { # Need to swap order of area_names to get row
          contrast_i <- paste0(area_names[a_c], " - ", area_names[a_r])
          df_row <- which(scores_posthoc_df$contrast == contrast_i)
        }
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

  # Finally, for purposes of reporting, we want the more informative "Label" for
  # the Area, not the shorthand, to show up in the final output.
  area_labels <- areas$Label[areas$Status == "Area"]
  names(area_labels) <- as.character(areas$Area[areas$Status == "Area"])
  
  # Re-order the area_labels vector so it appears in same order as row/columns
  # of post-hoc matrix
  area_labels <- area_labels[rownames(scores_posthoc_mat)]

  # Update row/column names in character matrix being written to file
  rownames(scores_posthoc_char) <- area_labels
  colnames(scores_posthoc_char) <- area_labels
  
  write.csv(x = scores_posthoc_char, 
            file = paste0("output/", framework, "-clmm-posthoc.csv"),
            row.names = TRUE)
  
  return(list(framework = framework,
              clmm_model = scores_clmm,
              posthoc = scores_posthoc))
}
