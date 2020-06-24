# Test for differences in area scores for GDS framework
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-06-05

rm(list = ls())

################################################################################
# Using a mixed model, where Area is fixed effect and program is random effect
source(file = "functions/run_nlme.R")

framework <- "gds"

run_nlme(framework = framework)
