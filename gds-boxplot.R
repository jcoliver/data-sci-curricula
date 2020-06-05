# Boxplot GDS framework scores
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-03-26

rm(list = ls())

################################################################################
library(tidyverse)
library(RColorBrewer)
source(file = "functions/boxplot_quantiles.R")

scores <- read.csv(file = "data/gds-scores.csv")
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
scores <- merge(x = scores, y = inst.names)

# Two columns are means of other columns
scores$GDS.Data.representation.modeling <- rowMeans(scores[, c("GDS.Databases", "GDS.Math")], na.rm = TRUE)
scores$GDS.Data.modeling <- rowMeans(scores[, c("GDS.Generative.Modeling", "GDS.Predictive.Modeling")], na.rm = TRUE)

# Drop some columns that aren't needed
drop.cols <- c("Row.Type", "GDS.Databases", "GDS.Math", 
               "GDS.Generative.Modeling", "GDS.Predictive.Modeling", "X", 
               "Site", "Date", "Notes")
scores <- scores[, -which(colnames(scores) %in% drop.cols)]

# Remove NA rows (i.e. UA B.S.)
scores <- na.omit(scores)

# Create column that is Institution X Program
scores$inst.prog <- paste0(scores$Short.name, 
                           " - ",
                           scores$Program)

# Convert to long for ease of plotting
scores.long <- scores %>%
  gather(key = "area",
         value = "score",
         -Institution, -Program, -Short.name, -inst.prog)

# Drop the prefix "GDS."
scores.long$area <- gsub(pattern = "GDS.", replacement = "", x = scores.long$area)

# Change some names for easier plotting
scores.long$area <- gsub(pattern = "Data.representation.modeling",
                         replacement = "Data.representation",
                         x = scores.long$area)
scores.long$area <- gsub(pattern = "Data.",
                         replacement = "Data\n",
                         x = scores.long$area)
scores.long$area <- gsub(pattern = "Science",
                         replacement = "Science about\nData Science",
                         x = scores.long$area)

# Relevel areas by scores and save summary stats
area.means <- scores.long %>%
  group_by(area) %>%
  summarize(mean = mean(score, na.rm = TRUE),
            sterr = sd(score, na.rm = TRUE)/sqrt(n()),
            median = median(score, na.rm = TRUE))
write.csv(x = area.means, 
          file = "output/table-scores-gds.csv",
          row.names = FALSE)
area.order <- area.means$area[order(area.means$mean, decreasing = TRUE)]
scores.long$area <- factor(x = scores.long$area, levels = rev(area.order))

getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

scores.boxplot <- ggplot(data = scores.long, mapping = aes(x = area, y = score)) +
  stat_summary(fun.data = boxplot_quantiles, 
               geom = "boxplot", 
               width = 0.4) +
  geom_point(position = "jitter",
             # mapping = aes(fill = Short.name),
             shape = 21, 
             alpha = 0.6) + 
  # scale_fill_manual(values = getPalette(length(unique(scores.long$Short.name))),
  #                   name = "Institution") +
  labs(x = "Key Concept",
       y = "Score") +
  theme_bw() +
  theme(legend.key.height = unit(x = 1, units = "picas")) +
  coord_flip()
ggsave(filename = "output/figure-scores-boxplot-gds.pdf",
       plot = scores.boxplot,
       width = 15,
       height = 10, 
       units = "cm")
