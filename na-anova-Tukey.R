# ANOVA and Tukey post-hoc tests on National Academies' framework
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-06-07

rm(list = ls())

# TODO: running tests on means (of each institution/program); going to want to 
# check this. May need to weight based on number of sub-areas used in 
# calculating area mean

################################################################################
library(tidyverse)
source(file = "functions/area_scores.R")

scores <- read.csv(file = "data/na-scores.csv", stringsAsFactors = FALSE)
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)
area.prefix <- "NA."

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
  # into separate columns
  scores.tukey[, c((ncol(scores.tukey) + 1):(ncol(scores.tukey) + 2))] <-
    str_split_fixed(string = scores.tukey$comparison, pattern = "-", n = 2)
  colnames(scores.tukey)[(ncol(scores.tukey) - 1):ncol(scores.tukey)]<-
    c("Area1", "Area2")

  # Would like a means of quickly assessing which areas are significantly 
  # different from others, a la the a, b, c convention, e.g.
  # Area                 A  B  C
  # Ethics               A
  # Domain                  B  
  # Math.foundations           C
  # Compute.foundations        C

  # Get means for ease of extracting information
  scores.means <- scores.long %>%
    group_by(area) %>%
    summarize(mean.score = mean(score)) %>%
    arrange(desc(mean.score))
  
  # keeps track of which letter we are on
  letter.index <- 1
  ns.previous <- NULL
  for (i in 1:length(scores.means$area)) {
    area <- scores.means$area[i]
    # Pull out only comparisons for current area
    comparisons <- scores.tukey[scores.tukey$Area1 == area | 
                                  scores.tukey$Area2 == area, ]
    # Restrict to those comparisons that are not significant
    ns.comparisons <- comparisons[comparisons$p.adj >= 0.05, ]
    
    # Make a vector of the area names for all n.s. comparisons
    ns.names <- c(ns.comparisons$Area1, ns.comparisons$Area2)
    # The current set of areas that are not significantly different
    ns.set <- unique(ns.names)
    new.set <- FALSE
    if (!is.null(ns.previous)) {
      if (!setequal(x = ns.set, y = ns.previous)) {
        new.set <- TRUE
      }
    } else {
      new.set <- TRUE
    }
    
    # This is a new set, so need to make new column and fill it in
    if (new.set) {
      scores.means[, LETTERS[letter.index]] <- FALSE
      scores.means[scores.means$area %in% ns.set, LETTERS[letter.index]] <- TRUE
      letter.index <- letter.index + 1
    }
    
    # Use ns.previous for next iteration of loop
    ns.previous <- ns.set
  }
}

