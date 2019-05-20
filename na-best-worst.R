# Best/worst areas covered in National Academies' framework
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-20

rm(list = ls())
################################################################################
library(tidyverse)

scores <- read.csv(file = "data/na-scores.csv", stringsAsFactors = FALSE)
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
scores <- merge(x = scores, y = inst.names)

areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)

# Drop any column that isn't a subarea (keeping institution and program info)
drop.cols <- areas$Key[areas$Status == "Area"]
drop.cols <- c(drop.cols, "Row.Type")
scores <- scores[, -which(colnames(scores) %in% drop.cols)]

# Remove NA rows (i.e. UA B.S.)
scores <- na.omit(scores)
rownames(scores) <- NULL

# Create column that is Institution X Program
scores$inst.prog <- paste0(scores$Short.name,
                           " - ", 
                           scores$Program)

# Convert to long for ease of wrangling
scores.long <- scores %>%
  gather(key = "subarea",
         value = "score",
         -Institution, -Program, -Short.name, -inst.prog)

# Calculate means for each sub-area
scores.mean <- scores.long %>%
  group_by(subarea) %>%
  summarise(mean.score = mean(score, na.rm = TRUE),
            perc.4 = 100 * sum(score == 4, na.rm = TRUE)/n(),
            perc.3 = 100 * sum(score == 3, na.rm = TRUE)/n(),
            perc.2 = 100 * sum(score == 2, na.rm = TRUE)/n(),
            perc.1 = 100 * sum(score == 1, na.rm = TRUE)/n())

# Clean up
scores.mean <- data.frame(scores.mean)
scores.mean[, c(2:6)] <- round(x = scores.mean[, c(2:6)], digits = 2)

# Add the Description and drop any resulting NA rows
scores.mean <- merge(x = scores.mean, by.x = "subarea",
                     y = areas[, c("Key", "Description")], by.y = "Key")

# Order in descending and extract best and worst
scores.mean <- scores.mean[order(scores.mean$mean.score, decreasing = TRUE), ]
rownames(scores.mean) <- NULL
cutoff <- 5 # Number of best/worst
scores.out <- scores.mean[c(1:5, c((nrow(scores.mean) - 4):nrow(scores.mean))), ]
write.csv(x = scores.out, file = "output/best-worst-na.csv", row.names = FALSE)
