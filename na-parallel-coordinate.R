# Parallel coordinate plot with National Academies' framework scores
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-08

rm(list = ls())

################################################################################
library(tidyverse)
library(RColorBrewer)

scores <- read.csv(file = "data/na-scores.csv", stringsAsFactors = FALSE)
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
scores <- merge(x = scores, y = inst.names)

areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)

# Need to calculate means for "Areas" from all contained "Subareas"
for (area in unique(x = areas$Area)) {
  # Look at one Area
  area.key <- areas$Key[areas$Area == area & areas$Status == "Area"]
  # Use Subarea to identify column names for calculating mean
  subarea.keys <- areas$Key[areas$Area == area & areas$Status == "Subarea"]
  # Catch situation when Area has only one Subarea (Domain)
  if (length(subarea.keys) > 1) {
    scores[, area.key] <- rowMeans(x = scores[, subarea.keys])
  } else {
    scores[, area.key] <- scores[, subarea.keys]
  }
}

# Now extract only those columns that have means
drop.cols <- areas$Key[areas$Status == "Subarea"]
drop.cols <- c(drop.cols, "Row.Type")
scores <- scores[, -which(colnames(scores) %in% drop.cols)]

# Remove NA rows (i.e. UA B.S.)
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
  ggtitle(label = "National Academies' framework") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5))
ggsave(filename = "output/na-scores-parallel.pdf")
