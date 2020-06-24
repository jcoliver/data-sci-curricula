# Test for differences in area scores for National Academies framework
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-06-03

rm(list = ls())

################################################################################
# Using a mixed model, where Area is fixed effect and program is random effect
source(file = "functions/run_nlme.R")

framework <- "na"

run_nlme(framework = framework)
