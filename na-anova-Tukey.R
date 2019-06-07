# ANOVA and Tukey post-hoc tests on National Academies' framework
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-06-07

rm(list = ls())

# TODO: running tests on means (of each institution/program); going to want to 
# check this

################################################################################
library(tidyverse)
# library(RColorBrewer)
source(file = "functions/area_scores.R")

scores <- read.csv(file = "data/na-scores.csv", stringsAsFactors = FALSE)
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)

# Calculate mean of each Area, based on scores of Subareas
scores <- area_scores(scores = scores, areas = areas)
scores <- merge(x = scores, y = inst.names)

# Remove NA rows (i.e. UA B.S.)
scores <- na.omit(scores)
rownames(scores) <- NULL

# Create column that is Institution X Program
scores$inst.prog <- paste0(scores$Short.name,
                           " - ", 
                           scores$Program)

# Convert to long for ease of statistics
scores.long <- scores %>%
  gather(key = "area",
         value = "score",
         -Institution, -Program, -Short.name, -inst.prog)

# Drop the prefix "NA." from area names
scores.long$area <- gsub(pattern = "NA.", replacement = "", x = scores.long$area)

# Run ANOVA
scores.aov <- aov(formula = score ~ area, data = scores.long)
aov.summary <- summary(scores.aov)[[1]]
area.significant <- aov.summary[1, 5] < 0.05
if (area.significant) {
  message("ANOVA significant for effect of Area on score")
  options(digits = 3) # only want to print out a few digits
  message(paste0("F = ", format(aov.summary[1, 4], digits = 4), 
                 "; p = ", format(aov.summary[1, 5], digits = 3)))
  options(digits = 7) # reset to default
} else {
  message("ANOVA not significant for effect of Area on score")
}

# If ANOVA was significant, run Tukey post-hoc test on pairwise comparisons
if (area.significant) {
  scores.tukey <- TukeyHSD(x = scores.aov, ordered = TRUE)[[1]]
}

# Would like a means of quickly assessing which areas are significantly 
# different from others