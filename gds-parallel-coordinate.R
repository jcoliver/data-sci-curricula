# Parallel coordinate plot with preliminary GDS data
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-08

rm(list = ls())

################################################################################
gds <- read.csv(file = "data/gds-scores.csv")
library(ggplot2)
library(tidyr)
library(dplyr)

# Two columns are means of other columns
gds$GDS.Data.representation.modeling <- rowMeans(gds[, c("GDS.Databases", "GDS.Math")], na.rm = TRUE)
gds$GDS.Data.modeling <- rowMeans(gds[, c("GDS.Generative.Modeling", "GDS.Predictive.Modeling")], na.rm = TRUE)

# Drop some columns that aren't needed
drop.cols <- c("Row.Type", "GDS.Databases", "GDS.Math", 
               "GDS.Generative.Modeling", "GDS.Predictive.Modeling", "X", 
               "Site", "Date", "Notes")
gds <- gds[, -which(colnames(gds) %in% drop.cols)]

# Remove NA rows (i.e. UA B.S.)
gds <- na.omit(gds)

# Create column that is Institution X Program
gds$inst.prog <- paste0(gds$Institution, gds$Program)

# Convert to long for ease of plotting
gds.long <- gds %>%
  gather(key = "area",
         value = "score",
         -Institution, -Program, -inst.prog)

# Drop the prefix "GDS."
gds.long$area <- gsub(pattern = "GDS.", replacement = "", x = gds.long$area)

# Change some names for easier plotting
gds.long$area <- gsub(pattern = "Data.representation.modeling",
                      replacement = "Data.representation",
                      x = gds.long$area)
gds.long$area <- gsub(pattern = "Data.",
                      replacement = "Data\n",
                      x = gds.long$area)

# Relevel areas by scores
area.means <- gds.long %>%
  group_by(area) %>%
  summarize(mean = mean(score, na.rm = TRUE))
area.order <- area.means$area[order(area.means$mean, decreasing = TRUE)]
gds.long$area <- factor(x = gds.long$area, levels = area.order)

ggplot(data = gds.long, mapping = aes(x = area, y = score,
                                      group = inst.prog, color = Institution)) +
  geom_line(position = position_dodge(width = 0.5), size = 0.5) +
  geom_point(position = position_dodge(width = 0.5), size = 1.6) +
  # Jitter for vertical axis won't work with two geoms because each has it's own
  # random vertical offset
  # geom_line(position = position_jitterdodge(dodge.width = 0.5, jitter.height = 0.1), size = 0.5) +
  # geom_point(position = position_jitterdodge(dodge.width = 0.5, jitter.height = 0.1), size = 1.6) + 
  theme_bw() +
  theme(legend.position = "none")
