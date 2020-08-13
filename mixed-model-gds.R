# Test for differences in area scores for GDS framework
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-06-05

rm(list = ls())

################################################################################
# Using a ordinal regression mixed model, where Area is fixed effect and 
# program is random effect
source(file = "functions/run_clmm.R")

framework <- "gds"

run_clmm(framework = framework)
