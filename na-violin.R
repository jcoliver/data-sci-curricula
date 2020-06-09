# Boxplot National Academies' framework scores
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-03-26

rm(list = ls())

# TODO: Update axis labels for areas to impart more meaning (i.e. align them 
# with names of areas as reported in National Academies' report)

################################################################################
library(tidyverse)
library(RColorBrewer)
source(file = "functions/area_scores.R")
source(file = "functions/boxplot_quantiles.R")

scores <- read.csv(file = "data/na-scores.csv", stringsAsFactors = FALSE)
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)
framework <- "na"

# Add also institutional data, which has value of Short.name
scores <- merge(x = scores, y = inst.names)

# Convert scores to long
scores_long <- scores %>%
  pivot_longer(names_to = "subarea",
               values_to = "score",
               cols = -c(Row.Type, Institution, Program, Short.name),
               values_drop_na = TRUE)

# Merge with the area data frame so we can line up subareas with appropriate 
# area
scores_long <- merge(x = scores_long,
                     y = areas[, c("Area", "Key")],
                     by.x = "subarea",
                     by.y = "Key")

# Renaming a column because I'm being pedantic
scores_long <- scores_long %>%
  rename(area = Area)

# Create column that is Institution X Program
scores_long$inst.prog <- paste0(scores_long$Short.name,
                                " - ", 
                                scores_long$Program)

# For long-formatted data, want to re-level area, from highest mean score to 
# lowest mean score

# Start by calculating summary statistics
area_stats <- scores_long %>%
  group_by(area) %>%
  summarize(mean_score = mean(score),
            sd_score = sd(score),
            median_score = median(score))

# Re-level area based on mean values
area_levels <- area_stats$area[order(area_stats$mean_score)]
scores_long$area <- factor(x = scores_long$area,
                           levels = area_levels)

# Write the summary stats to a file while we are here
write.csv(x = area_stats, 
          file = paste0("output/table-scores-", framework, ".csv"),
          row.names = FALSE)

# Create means for each program X institution combination so we can add points 
# to boxplot
program_means <- scores_long %>%
  group_by(Short.name, Program, area) %>%
  summarize(score = mean(score))

# Plot the scores; geom_violin uses scores_long data, while geom_point uses 
# program_means. Not convinced the coloring of quantiles will work in print, 
# since outline is being drawn more than once.
scores_violin <- ggplot(data = scores_long, mapping = aes(x = area, y = score)) +
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_violin(color = NA) +
  geom_violin(draw_quantiles = c(0.25, 0.75),
              fill = NA,
              color = "blue",
              size = 0.5,
              linetype = "11") +
  # scale_x_discrete(name = "Area",
  #                  limits = scores_long$area,
  #                  labels = scores_long$Description) +
  geom_violin(draw_quantiles = c(0.5),
              fill = NA) +
  geom_point(data = program_means,
             mapping = aes(x = area, y = score),
             position = position_jitter(width = 0.2),
             shape = 21, 
             alpha = 0.6) + 
  labs(x = "Area",
       y = "Score") +
  theme_bw() +
  coord_flip()
scores_violin

ggsave(filename = paste0("output/figure-scores-violin-", framework, ".pdf"),
       plot = scores_violin,
       width = 10,
       height = 10, 
       units = "cm")
