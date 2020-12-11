# Comparing scorings for GDS framework
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-04-01

################################################################################
# Before reading in tm scores, need to drop all rows that aren't the 
# 4-point scale code (using just column names from JO scoresheet)
# in the data directory:
# head -n1 na-01-tm-raw.csv > na-01-tm.csv
# grep -e "4-point scale code" na-01-tm-raw.csv >> na-01-tm.csv
tm.scores <- read.csv(file = "data/preliminary-scores/na-01-tm.csv")
jo.scores <- read.csv(file = "data/preliminary-scores/na-01-jo.csv")

# Sort the data.frames to ensure same order
tm.scores <- tm.scores[order(tm.scores$Institution, tm.scores$Program), ]
jo.scores <- jo.scores[order(jo.scores$Institution, jo.scores$Program), ]

# And reality check to ensure they are the same
total.matching <- sum(paste0(tm.scores$Institution, tm.scores$Program) == 
                        paste0(jo.scores$Institution, jo.scores$Program))
if (nrow(tm.scores) - total.matching != 0) {
  message("Mismatch in data order.")
}

# Columns of interest (some columns only have NA values)
score.cols <- c(4:ncol(tm.scores))

# Create a data frame that will hold results of comparison
comparisons <- tm.scores
comparisons[, score.cols] <- tm.scores[, score.cols] == jo.scores[, score.cols]

# # Finally, use those comparisons to replace differing scores with NA
# tm.scores.diff <- tm.scores[, score.cols]
# tm.scores.diff[, score.cols]

# Send to a CSV file that we can use for re-checking
write.csv(x = comparisons, 
          file = "output/na-comparisons-01.csv", 
          row.names = FALSE)

# And for reporting purposes, also calculate the mean difference
tm.scores.matrix <- as.matrix(tm.scores[, score.cols])
jo.scores.matrix <- as.matrix(jo.scores[, score.cols])
mean.diff <- mean(tm.scores.matrix - jo.scores.matrix, na.rm = TRUE)
