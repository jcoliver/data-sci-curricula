# Clean up data files
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-07-24

rm(list = ls())

################################################################################
stop("Script has already been run. Data files already cleaned up.")

# The files with scores include columns that lack any data and a column called 
# Row.Type, which is not useful for any analyses. Need to drop the NA columns
# and the Row.Type column, then update any files referring to those columns
library(tidyverse)

frameworks <- c("na", "gds")

for (framework in frameworks) {
  score_file <- paste0("data/scores-", framework, ".csv")
  scores <- read.csv(file = score_file, stringsAsFactors = FALSE)

  areas_file <- paste0("data/areas-key-", framework, ".csv")
  areas <- read.csv(file = areas_file, stringsAsFactors = FALSE)
  
  # Remove Row.Type 
  scores <- scores %>%
    select(-Row.Type)
  
  # Identify those columns that have NAs
  cols_drop <- is.na(scores[1, ])
  
  scores <- scores[, !cols_drop]
  
  write.csv(x = scores, file = score_file, row.names = FALSE)
}
