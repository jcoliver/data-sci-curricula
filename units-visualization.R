# Units visualization for undergraduate majors
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-06-07

rm(list = ls())

# TODO: Will need to come up with means of referencing Institution/Program 
# combinations in plot that doesn't take up too much horizontal space
################################################################################
library(tidyverse)

units.df <- read.csv(file = "data/units.csv")
programs <- read.csv(file = "data/programs.csv")
institutions <- read.csv(file = "data/institution-names.csv")

# Merge the institutions with units to get short names in units.df
units.df <- merge(x = units.df, y = institutions)
rm(institutions)

# Limit units df to those programs that are majors (exclude minor & certificate
# programs)
majors <- programs[programs$Major, ]
units.df <- units.df[units.df$Program %in% majors$Program, ]
rm(programs, majors)

# Drop any NA rows
units.df <- na.omit(units.df)

# Calculate midpoint values between min/max to use for plot
# Stats, CS, Domain
units.df$Stats <- (units.df$Stats.min + units.df$Stats.max) / 2
units.df$CS <- (units.df$CS.min + units.df$CS.max) / 2
units.df$Domain <- (units.df$Domain.min + units.df$Domain.max) / 2
units.df$Total <- units.df$Stats + units.df$CS + units.df$Domain

# Calculate values as proportion of total units/hours for major
units.df$Stats.prop <- units.df$Stats / units.df$Total
units.df$CS.prop <- units.df$CS / units.df$Total
units.df$Domain.prop <- units.df$Domain / units.df$Total

# Transform to long format for plotting
units.long <- units.df %>%
  select(Institution, Program, Short.name, Stats.prop, CS.prop, Domain.prop) %>%
  gather(key = "Area", value = "Proportion", -Institution, -Program, -Short.name)

# Make Area values a little more human readable
units.long$Area <- gsub(pattern = ".prop", replacement = "", units.long$Area)
units.long$Area <- gsub(pattern = "Stats",
                        replacement = "Statistics/Mathematics",
                        units.long$Area)
units.long$Area <- gsub(pattern = "CS",
                        replacement = "Computer Science",
                        units.long$Area)

# Making additional field for plot labels
units.long$Plot.label <- paste0(units.long$Short.name, "\n", units.long$Program)

# Re-leveling to display in Stats, CS, Domain order
units.long$Area <- factor(units.long$Area, levels = c("Domain", 
                                                      "Computer Science",
                                                      "Statistics/Mathematics"))

# Plot as stacked bar plot
units.plot <- ggplot(data = units.long, mapping = aes(x = Plot.label, 
                                                      y = Proportion,
                                                      fill = Area)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Institution / Program", y = "Proportion of Major") +
  scale_fill_manual(values = c("#D2D2D2", "#222222", "#8F8F8F")) +
  theme_bw()
print(units.plot)
ggsave(filename = "output/program-units.pdf", 
       plot = units.plot)
