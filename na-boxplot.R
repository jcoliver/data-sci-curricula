# Boxplot National Academies' framework scores
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-03-26

rm(list = ls())

################################################################################
library(tidyverse)
library(RColorBrewer)
source(file = "functions/area_scores.R")
source(file = "functions/boxplot_quantiles.R")

scores <- read.csv(file = "data/na-scores.csv", stringsAsFactors = FALSE)
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)

# Calculate mean of each Area, based on scores of Subareas
scores <- area_scores(scores = scores, areas = areas)
scores <- merge(x = scores, y = inst.names)

# Remove NA rows
scores <- na.omit(scores)
rownames(scores) <- NULL

# Create column that is Institution X Program
scores$inst.prog <- paste0(scores$Short.name,
                           " - ", 
                           scores$Program)

# Convert to long for ease of plotting
scores.long <- scores %>%
  gather(key = "area",
         value = "score",
         -Institution, -Program, -Short.name, -inst.prog)

# Drop the prefix "NA." from area names
scores.long$area <- gsub(pattern = "NA.", replacement = "", x = scores.long$area)

# Change some names for easier plotting
scores.long$area <- gsub(pattern = "\\.", replacement = " ", x = scores.long$area)
scores.long$area <- gsub(pattern = "Math foundations", 
                     replacement = "Mathematical\nfoundations",
                     x = scores.long$area)
scores.long$area <- gsub(pattern = "Compute foundations", 
                     replacement = "Computational\nfoundations",
                     x = scores.long$area)
scores.long$area <- gsub(pattern = "Statistics foundations", 
                     replacement = "Statistical\nfoundations",
                     x = scores.long$area)
scores.long$area <- gsub(pattern = "management curation", 
                     replacement = "management\n& curation",
                     x = scores.long$area)
scores.long$area <- gsub(pattern = "description visualization",
                     replacement = "description\n& visualization", 
                     x = scores.long$area)
scores.long$area <- gsub(pattern = "modeling assessment",
                     replacement = "modeling\n& assessment", 
                     x = scores.long$area)
scores.long$area <- gsub(pattern = "Workflow reproducibility",
                     replacement = "Reproducibility", 
                     x = scores.long$area)
scores.long$area <- gsub(pattern = " teamwork",
                     replacement = "\n& teamwork", 
                     x = scores.long$area)
scores.long$area <- gsub(pattern = "Domain",
                     replacement = "Domain-specific\neducation", 
                     x = scores.long$area)

# Relevel areas by scores and save summary stats
area.means <- scores.long %>%
  group_by(area) %>%
  summarize(mean = mean(score, na.rm = TRUE),
            sterr = sd(score, na.rm = TRUE)/sqrt(n()),
            median = median(score, na.rm = TRUE))
write.csv(x = area.means, 
          file = "output/table-scores-na.csv",
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
ggsave(filename = "output/figure-scores-boxplot-na.pdf",
       plot = scores.boxplot,
       width = 15,
       height = 10, 
       units = "cm")
