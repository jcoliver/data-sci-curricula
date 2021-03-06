# Comparing scorings for GDS framework between JCO second and TM first
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-04-25

################################################################################
# See gds-compare-01.R for selection of relevant rows from data/gds-01-tm-raw.csv
tm.scores <- read.csv(file = "data/preliminary-scores/gds-01-tm.csv")
jo.scores <- read.csv(file = "data/preliminary-scores/gds-02-jo.csv")

# Drop the extra columns in the jo file
jo.scores <- jo.scores[, -c(14:17)]

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
score.cols <- c(4:13)

# Create a data frame that will hold results of comparison
comparisons <- tm.scores
comparisons[, score.cols] <- tm.scores[, score.cols] == jo.scores[, score.cols]

# Use logical math to replace differening values with zeros
tm.conflict <- tm.scores
tm.conflict[, score.cols] <- tm.scores[, score.cols] * comparisons[, score.cols]
tm.conflict[tm.conflict == 0] <- -9

jo.conflict <- jo.scores
jo.conflict[, score.cols] <- jo.scores[, score.cols] * comparisons[, score.cols]
jo.conflict[jo.conflict == 0] <- -9

# Send to CSV files that we can use for re-checking
write.csv(x = comparisons, 
          file = "output/gds-comparisons-01b.csv", 
          row.names = FALSE)
write.csv(x = tm.conflict, 
          file = "output/gds-01-tm-conflicts.csv", 
          row.names = FALSE)
write.csv(x = jo.conflict, 
          file = "output/gds-02-jo-conflicts.csv", 
          row.names = FALSE)

# And for reporting purposes, also calculate the mean difference
tm.scores.matrix <- as.matrix(tm.scores[, score.cols])
jo.scores.matrix <- as.matrix(jo.scores[, score.cols])
mean.diff <- mean(tm.scores.matrix - jo.scores.matrix, na.rm = TRUE)
