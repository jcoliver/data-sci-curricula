# ANOVA and Tukey post-hoc tests on Greater Data Science framework
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-06-12

rm(list = ls())

# TODO: running tests on means (of each institution/program); going to want to 
# check this

################################################################################
library(tidyverse)
source(file = "functions/area_scores.R")

scores <- read.csv(file = "data/gds-scores.csv", stringsAsFactors = FALSE)
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
areas <- read.csv(file = "data/gds-areas-key.csv", stringsAsFactors = FALSE)
area.prefix <- "GDS."

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

# Drop the framework prefix from area names
scores.long$area <- gsub(pattern = area.prefix, 
                         replacement = "", 
                         x = scores.long$area)

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
  scores.tukey <- data.frame(scores.tukey)
  scores.tukey$comparison <- rownames(scores.tukey)
  rownames(scores.tukey) <- NULL
  # Some data wrangling to make it easier to visualize; splitting comparison
  # Into separate columns
  scores.tukey[, c((ncol(scores.tukey) + 1):(ncol(scores.tukey) + 2))] <-
    str_split_fixed(string = scores.tukey$comparison, pattern = "-", n = 2)
  colnames(scores.tukey)[(ncol(scores.tukey) - 1):ncol(scores.tukey)]<-
    c("Area1", "Area2")

} else {
  message("Omnibus ANOVA not significant; no post-hoc comparisons made.")
}

