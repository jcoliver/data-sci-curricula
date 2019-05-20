# Parallel coordinate plot with preliminary GDS data
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-08

rm(list = ls())

################################################################################
library(tidyverse)
library(RColorBrewer)

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

# Relevel areas by scores
area.means <- scores.long %>%
  group_by(area) %>%
  summarize(mean = mean(score, na.rm = TRUE),
            sterr = sd(score, na.rm = TRUE)/sqrt(n()))
area.order <- area.means$area[order(area.means$mean, decreasing = TRUE)]
scores.long$area <- factor(x = scores.long$area, levels = area.order)

getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

ggplot(data = scores.long, mapping = aes(x = area)) +
  geom_line(size = 0.5, alpha = 0.7, mapping = aes(y = score, 
                                                   color = Short.name, 
                                                   group = inst.prog)) +
  geom_point(data = area.means, mapping = aes(y = mean)) +
  geom_errorbar(data = area.means, mapping = aes(ymin = (mean - sterr),
                                                 ymax = (mean + sterr)),
                width = 0.2) +
  scale_color_manual(values = getPalette(length(unique(scores.long$Short.name))),
                     name = "Institution") +
  xlab(label = "Key Concept") +
  ylab(label = "Score") + 
  ggtitle(label = "Greater Data Science framework") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5))
ggsave(filename = "output/figure-scores-parallel-gds.pdf")
