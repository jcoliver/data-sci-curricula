# Caterpillar plot of National Academies' framework scores
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-06-12

rm(list = ls())

################################################################################
source(file = "functions/catplot.R")

framework <- "na"
plot_height_mod <- 1

catplot(framework = framework, 
        plot_height_mod = plot_height_mod)
