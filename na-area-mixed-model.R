# Test for differences in area scores for National Academies framework
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-06-03

rm(list = ls())

################################################################################
# Using a mixed model, where Area is fixed effect and program is random effect
library(tidyr)
library(dplyr)
library(lmerTest)
library(emmeans)  # post-hoc test

scores <- read.csv(file = "data/na-scores.csv", stringsAsFactors = FALSE)
areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)

# Convert scores to long
# Skip Row.Type, Institution, and Program
# During pivot, drop data from columns with higher area name, as they have no 
# data
scores_long <- scores %>%
  pivot_longer(cols = starts_with(match = "NA."),
               names_to = "Subarea", 
               values_to = "Score",
               values_drop_na = TRUE)

# Need to assign Area for each subarea.
scores_long <- merge(x = scores_long, 
                     y = areas[, c("Area", "Key")],
                     by.x = "Subarea",
                     by.y = "Key")

# Run mixed model: Area as fixed, program as random 
scores_lme <- lmerTest::lmer(Score ~ Area + (1|Program), 
                         data = scores_long)
anova(scores_lme) # shows an effect of Area

# Post-hoc with multcomp, using Holm 1979 correction for multiple comparisons
scores_posthoc <- summary(multcomp::glht(scores_lme, 
                                         linfct = multcomp::mcp(Area = "Tukey")),
                          test = multcomp::adjusted("holm"))

# Build a table from which we can extract coefficient estimates and p-values
scores_posthoc_df <- data.frame(comparison = names(scores_posthoc$test$coefficients),
                                   coefficients = scores_posthoc$test$coefficients,
                                   sigma = scores_posthoc$test$sigma,
                                   t_value = scores_posthoc$test$tstat,
                                   p_value = scores_posthoc$test$pvalues)
rownames(scores_posthoc_df) <- NULL

scores_posthoc_df <- scores_posthoc_df[order(scores_posthoc_df$p_value), ]
