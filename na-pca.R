# National Academies' framework PCA
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-15

rm(list = ls())

################################################################################
# Data wrangling
na.scores <- read.csv(file = "data/na-scores.csv", stringsAsFactors = FALSE)
na.areas <- read.csv(file = "data/na-areas-key.csv", stringsAsFactors = FALSE)
inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)

# Replace institution with shorter names for plotting
for (inst in na.scores$Institution) {
  short.name <- inst.names$Short.name[inst.names$Institution == inst]
  na.scores$Institution <- gsub(pattern = inst,
                                replacement = short.name,
                                x = na.scores$Institution)
}

# Drop "Areas" columns
drop.cols <- na.areas$Key[na.areas$Status == "Area"]
drop.cols <- c(drop.cols, "Row.Type")
na.scores <- na.scores[, -(which(colnames(na.scores) %in% drop.cols))]

# Remove NA rows (i.e. UA B.S.)
na.scores <- na.omit(na.scores)
rownames(na.scores) <- NULL

################################################################################
# Principal Components Analysis
score.matrix <- as.matrix(x = na.scores[, -(which(colnames(na.scores) %in% c("Institution", "Program")))])

# TODO: Need to remove columns that are invariant (i.e. have a sd of zero)
score.sd <- apply(X = score.matrix, MARGIN = 2, FUN = sd)
invariant <- which(score.sd == 0)
score.matrix <- score.matrix[, -invariant]

na.pca <- prcomp(x = score.matrix, center = TRUE, scale. = TRUE)
summary(na.pca)
plot(na.pca)

################################################################################
# Clustering
