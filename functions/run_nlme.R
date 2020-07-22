#' Run linear mixed-effect model on area scores
#' 
#' @param framework  character vector indicating which framework to analyze
run_nlme <- function(framework = c("na", "gds")) {
  message("run_nlme is deprecated; use run_clmm instead.")
  stop()
  
  if(!require(car)) {
    stop("run_nlme requires car package, which could not be loaded.")
  }
  if(!require(tidyr)) {
    stop("run_nlme requires tidyr package, which could not be loaded.")
  }
  if(!require(dplyr)) {
    stop("run_nlme requires dplyr package, which could not be loaded.")
  }
  if(!require(lmerTest)) {
    stop("run_nlme requires lmerTest package, which could not be loaded.")
  }
  if(!require(emmeans)) {
    stop("run_nlme requires emmeans package, which could not be loaded.")
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
  
  # Run mixed model: Area as fixed, program as random 
  
  scores_lme <- lmerTest::lmer(Score ~ Area + (1|Program), 
                               data = scores_long)
  lme_model <- anova(scores_lme) # shows an effect of Area
  
  # Test assumptions of model:
  # 1. Linearity: Plot response (y) vs. residuals (x)
  # 2a. Homoscedasticity: Plot standardized residuals (y) vs predicted values (x)
  # 2b. Homoscedasticity: Run Levene's test
  # 3. Normality: Q-Q plot
  
  assumptions_file <- paste0("output/", framework, "-assumptions.pdf")
  pdf(file = assumptions_file, useDingbats = FALSE)
  # 1. Linearity: Plot response (y) vs. residuals (x)
  plot(x = resid(scores_lme),
       y = scores_long$Score,
       xlab = "Residuals from LME model",
       ylab = paste0("Scores, ", framework, " framework"))

  # 2a. Homoscedasticity: Plot standardized residuals (y) vs predicted values (x)
    plot(scores_lme,
       xlab = "Fitted scores",
       ylab = "Standardized residuals")
    
  # 2b. Homoscedasticity: Run Levene's test
  # leveneTest(residuals(scores_lme) ~ scores_long$Area)
  
  # 3. Normality: Q-Q plot
  lattice::qqmath(scores_lme)
  
  dev.off()

  # Write model results to file, and if appropriate, run post-hoc pairwise 
  # comparisons
  output_file <- paste0("output/", framework, "-lme.txt")
  sink(file = output_file)
  cat("Linear mixed effects model results\n\n")
  print(lme_model)
  
  run_post_hoc <- FALSE
  if (lme_model$`Pr(>F)`[1] < 0.05) {
    run_post_hoc <- TRUE
  } else {
    message("No significant effect of Area on score found.")
  }
  
  if (run_post_hoc) {
    # Post-hoc with multcomp, using Holm 1979 correction for multiple comparisons
    scores_posthoc <- summary(multcomp::glht(scores_lme, 
                                             linfct = multcomp::mcp(Area = "Tukey")),
                              test = multcomp::adjusted("holm"))
    cat("\nCompact letter display of post-hoc comparisons:\n")
    posthoc_letters <- multcomp::cld(scores_posthoc)
    
    # For saving & printing, need to remove newline characters from Area names
    print(posthoc_letters)
    
    # Write the letters to a file for use in plotting, but first make it a data
    # frame
    posthoc_df <- data.frame(Area = names(posthoc_letters$mcletters$Letters),
                             Letters = posthoc_letters$mcletters$Letters)
    
    letters_file <- paste0("output/", framework, "-lme-letters.csv")
    write.csv(x = posthoc_df, file = letters_file, row.names = FALSE)
    
    # Build a table from which we can extract coefficient estimates and p-values
    scores_posthoc_df <- data.frame(comparison = names(scores_posthoc$test$coefficients),
                                    coefficients = scores_posthoc$test$coefficients,
                                    sigma = scores_posthoc$test$sigma,
                                    t_value = scores_posthoc$test$tstat,
                                    p_value = scores_posthoc$test$pvalues)
    rownames(scores_posthoc_df) <- NULL
  }
  sink()
}
