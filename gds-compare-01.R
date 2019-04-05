# Comparing scorings for GDS framework
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-04-01

rm(list = ls())

# TODO: For second comparison
# Don't output TRUE/FALSE matrix, rather, for every cell that differs, replace 
# scores with an NA, and write this to a CV file. Can then format all NA rows 
# to be colored to highlight need for attention.

################################################################################
# Before reading in tm scores, need to drop all rows that aren't the 
# 4-point scale code (using just column names from JO scoresheet)
# in the data directory:
# $ head -n1 gds-01-tm-raw.csv > gds-01-tm.csv
# $ grep -e "4-point scale code" gds-01-tm-raw.csv >> gds-01-tm.csv
tm.scores <- read.csv(file = "data/gds-01-tm.csv")
jo.scores <- read.csv(file = "data/gds-01-jo.csv")

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

# Send to a CSV file that we can use for re-checking
write.csv(x = comparisons, file = "output/gds-comparisons-01.csv", row.names = FALSE)

# And for reporting purposes, also calculate the mean difference
tm.scores.matrix <- as.matrix(tm.scores[, score.cols])
jo.scores.matrix <- as.matrix(jo.scores[, score.cols])
mean.diff <- mean(tm.scores.matrix - jo.scores.matrix, na.rm = TRUE)
