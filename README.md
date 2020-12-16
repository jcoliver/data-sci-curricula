# Data science curricula comparisons

An investigation of how undergraduate data science programs perform against two 
data science frameworks, the National Academies' "Data Science for 
Undergraduates Opportunities and Options" (2018) and Donoho's "Greater Data
Science" (2017).

## Dependencies

The analyses and visualizations require the following packages:

+ dplyr (part of tidyverse)
+ emmeans
+ ggplot2 (part of tidyverse)
+ ordinal
+ stringr (part of tidyverse)
+ tidyr (part of tidyverse)
+ tidyverse

## Workflow

This work addresses two main questions:

1. Which areas of study are well-covered by undergraduate data science programs 
and which areas of study are lacking?
2. Does the domain of the administrative unit (i.e. department or college) in 
which the program is housed influenced the proportion of coursework in computer 
science, mathematics, and statistics?

### 1. Areas of study

To evaluate programs relative to the two frameworks, the scripts 
mixed-model-<framework>.R, run mixed-effects ordinal regression to compare 
coverage of the Areas in each framework.

```{r mixed-model-comparions}
# Compare coverage for the National Academies' framework
source(file = "mixed-model-nasem.R")

# Compare coverage for the Greater Data Science framework
source(file = "mixed-model-gds.R")
```

To visually compare coverage among areas, the scripts 
scores-<framework>-catplot.R produce caterpillar plots of the scores of 
different areas

```{r visualize-area-coverage}
# Caterpillar plot for National Academies' framework
source(file = "scores-nasem-catplot.R")

# Caterpillar plot for Greater Data Science framework
source(file = "scores-gds-catplot.R")
```

### 2. Coursework and administrative unit

To test for an influence on home unit on the relative amount of coursework in 
computer science and statistics/mathematics, we used Student's _t_ tests to 
address each hypothesis:

1. Programs housed in computer science units had more required coursework in
computer science than did programs housed in other academic units.
2. Programs housed in statistics or mathematics units had more required 
coursework in statistics and mathematics than did programs housed in other 
academic units.

The script units-statistics.R runs these tests.

```{r units-statistics}
source(file = "units-statistics.R")
```

To visualize the proportion of coursework in three areas (computer science, 
statistics/mathematics, domain), the units-visualization.R script creates a 
parallel coordinates plot with the proportions for each undergraduate data 
science program analyzed here.

```{r units-visualization}
source(file = "units-visualization.R")
```
