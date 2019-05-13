# Parallel coordinate plot with preliminary GDS data
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-08

rm(list = ls())

################################################################################
library(ggplot2)
library(tidyr)
library(dplyr)

na.scores <- read.csv(file = "data/na-scores.csv")
na.areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)

# Need to calculate means for "Areas" from all contained "Subareas"
for (area in unique(x = na.areas$Area)) {
  # Look at one Area
  area.key <- na.areas$Key[na.areas$Area == area & na.areas$Status == "Area"]
  # Use Subarea to identify column names for calculating mean
  subarea.keys <- na.areas$Key[na.areas$Area == area & na.areas$Status == "Subarea"]
  # Catch situation when Area has only one Subarea (Domain)
  if (length(subarea.keys) > 1) {
    na.scores[, area.key] <- rowMeans(x = na.scores[, subarea.keys])
  } else {
    na.scores[, area.key] <- na.scores[, subarea.keys]
  }
}

# Now extract only those columns that have means
drop.cols <- na.areas$Key[na.areas$Status == "Subarea"]
drop.cols <- c(drop.cols, "Row.Type")
na.scores <- na.scores[, -which(colnames(na.scores) %in% drop.cols)]

# Remove NA rows (i.e. UA B.S.)
na.scores <- na.omit(na.scores)

# Create column that is Institution X Program
na.scores$inst.prog <- paste0(na.scores$Institution, 
                              na.scores$Program)

# Convert to long for ease of plotting
na.long <- na.scores %>%
  gather(key = "area",
         value = "score",
         -Institution, -Program, -inst.prog)

# Drop the prefix "NA."
na.long$area <- gsub(pattern = "NA.", replacement = "", x = na.long$area)

# Change some names for easier plotting
na.long$area <- gsub(pattern = "\\.", replacement = " ", x = na.long$area)
na.long$area <- gsub(pattern = "Math foundations", 
                     replacement = "Mathematical\nfoundations",
                     x = na.long$area)
na.long$area <- gsub(pattern = "Compute foundations", 
                     replacement = "Computational\nfoundations",
                     x = na.long$area)
na.long$area <- gsub(pattern = "Statistics foundations", 
                     replacement = "Statistical\nfoundations",
                     x = na.long$area)
na.long$area <- gsub(pattern = "management curation", 
                     replacement = "management\n& curation",
                     x = na.long$area)
na.long$area <- gsub(pattern = "description visualization",
                     replacement = "description\n& visualization", 
                     x = na.long$area)
na.long$area <- gsub(pattern = "modeling assessment",
                     replacement = "modeling\n& assessment", 
                     x = na.long$area)
na.long$area <- gsub(pattern = "Workflow reproducibility",
                     replacement = "Reproducibility", 
                     x = na.long$area)
na.long$area <- gsub(pattern = " teamwork",
                     replacement = "\n& teamwork", 
                     x = na.long$area)
na.long$area <- gsub(pattern = "Domain",
                     replacement = "Domain-specific", 
                     x = na.long$area)

# Relevel areas by scores
area.means <- na.long %>%
  group_by(area) %>%
  summarize(mean = mean(score, na.rm = TRUE))
area.order <- area.means$area[order(area.means$mean, decreasing = TRUE)]
na.long$area <- factor(x = na.long$area, levels = area.order)

ggplot(data = na.long, mapping = aes(x = area, y = score,
                                      group = inst.prog, color = Institution)) +
  geom_line(size = 0.25) +
  geom_point(size = 1.6) +
  xlab(label = "Key Concept") +
  ylab(label = "Score") + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 50, vjust = 0.5))
