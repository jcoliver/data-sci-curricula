# Parallel coordinate plot with preliminary GDS data
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-02-05

rm(list = ls())

################################################################################
gds <- read.csv(file = "data/gds.csv")
library(ggplot2)
library(tidyr)

# Create column that is Institution X Program
gds$inst.prog <- paste0(gds$Institution, gds$Program)

# Convert to long for ease of plotting
gds.long <- gds %>%
  gather(key = "area",
         value = "score",
         -Institution, -Program, -inst.prog)

# Drop the prefix "GDS."
gds.long$area <- gsub(pattern = "GDS.", replacement = "", x = gds.long$area)

#TODO: should probably order levels of area by score
\
ggplot(data = gds.long, mapping = aes(x = area, y = score,
                                      group = inst.prog, color = Institution)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5))
