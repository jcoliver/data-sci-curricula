# Caterpillar plot of Greater Data Science framework scores
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-06-12

################################################################################
source(file = "functions/catplot.R")

framework <- "gds"
plot_height_mod <- 3

catplot(framework = framework, 
        plot_height_mod = plot_height_mod)

rm(framework, plot_height_mod)