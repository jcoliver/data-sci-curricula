# Test for differences in proportion based on program home unit
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-03-27

################################################################################
library(tidyverse)

# Summary statistics and statistical test comparing units among programs
# Provides mean proportion of CS, math/stats, and domain units and addresses 
# two questions:
# 1. Do programs housed in Computer Science units have a higher proportion of 
# computer science courses than programs housed in other units?
# 2. Do programs housed in Statistics units have a higher proportion of 
# stats/math courses than programs housed in other units?

units.df <- read.csv(file = "data/units.csv")
programs <- read.csv(file = "data/programs.csv")
institutions <- read.csv(file = "data/institution-names.csv")

# Merge the institutions with units to get short names in units.df
units.df <- merge(x = units.df, y = institutions)
rm(institutions)

# Merge units.df with programs
units.df <- merge(x = units.df, y = programs)
rm(programs)

# Limit units df to those programs that are majors (exclude minor & certificate
# programs)
units.df <- units.df[units.df$Major, ]

# Drop any NA rows
units.df <- na.omit(units.df)

# Re-number rows
rownames(units.df) <- NULL

# Calculate midpoint values between min/max to use for plot
# Stats, CS, Domain
units.df$Stats <- (units.df$Stats.min + units.df$Stats.max) / 2
units.df$CS <- (units.df$CS.min + units.df$CS.max) / 2
units.df$Domain <- (units.df$Domain.min + units.df$Domain.max) / 2
units.df$Total <- (units.df$Major.min + units.df$Major.max) / 2

# Calculate values as proportion of total units/hours for major
units.df$Stats.prop <- units.df$Stats / units.df$Total
units.df$CS.prop <- units.df$CS / units.df$Total
units.df$Domain.prop <- units.df$Domain / units.df$Total

# Create separate columns for CS home and Stats home (because some programs are 
# housed in multiple units)
units.df$CS.home <- FALSE
units.df$CS.home[grep(x = units.df$Home.unit.category, pattern = "cs")] <- TRUE
units.df$Stats.home <- FALSE
units.df$Stats.home[grep(x = units.df$Home.unit.category, pattern = "math_stats")] <- TRUE

# Summary statistics
summary.stats <- data.frame(category = c("CS", "Stats", "Domain"),
                            mean.prop = c(mean(units.df$CS.prop),
                                          mean(units.df$Stats.prop),
                                          mean(units.df$Domain.prop)),
                            min.prop = c(min(units.df$CS.prop),
                                         min(units.df$Stats.prop),
                                         min(units.df$Domain.prop)),
                            max.prop = c(max(units.df$CS.prop),
                                         max(units.df$Stats.prop),
                                         max(units.df$Domain.prop)))
summary.stats[, c(2:4)] <- round(x = summary.stats[, c(2:4)], digits = 3)

# t-test 1. More CS courses in CS-housed programs?
cs.t <- t.test(x = units.df$CS.prop[units.df$CS.home],
               y = units.df$CS.prop[!units.df$CS.home],
               alternative = "greater")
# t-test 2. More Math/stats courses in Math/stats-housed programs?
stat.t <- t.test(x = units.df$Stats.prop[units.df$Stats.home],
                 y = units.df$Stats.prop[!units.df$Stats.home],
                 alternative = "greater")

# Write outputs to file
if (!dir.exists(paths = "output")) {
  dir.create("output")
}

sink(file = "output/units-statistics.txt")
cat("Summary statistics", "\n")
summary.stats
cat("\n========================================", "\n")
cat("Computer Science test", "\n")
cs.t
cat("========================================", "\n")
cat("Math/Stats test", "\n")
stat.t
sink()
