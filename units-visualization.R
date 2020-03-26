# Units visualization for undergraduate majors
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-06-07

rm(list = ls())

################################################################################
library(tidyverse)

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
# units.df$Total <- units.df$Stats + units.df$CS + units.df$Domain

# Calculate values as proportion of total units/hours for major
units.df$Stats.prop <- units.df$Stats / units.df$Total
units.df$CS.prop <- units.df$CS / units.df$Total
units.df$Domain.prop <- units.df$Domain / units.df$Total

# Transform to long format for plotting
units.long <- units.df %>%
  select(Institution, Program, Short.name, Stats.prop, CS.prop, Domain.prop, Program.Abbr, Home.unit.category) %>%
  gather(key = "Area", value = "Proportion", -Institution, -Program, -Short.name, -Program.Abbr, -Home.unit.category)

# Make Area values a little more human readable
units.long$Area <- gsub(pattern = ".prop", replacement = "", units.long$Area)
units.long$Area <- gsub(pattern = "Stats",
                        replacement = "Statistics/Mathematics",
                        units.long$Area)
units.long$Area <- gsub(pattern = "CS",
                        replacement = "Computer Science",
                        units.long$Area)

# Making additional field for plot labels
units.long$Plot.label <- paste0(units.long$Short.name, "\n", units.long$Program.Abbr)

# Relevel Plot.label so they are ordered by the category of the home department
units.long <- units.long[order(units.long$Home.unit.category, units.long$Institution), ]
units.long$Plot.label <- factor(units.long$Plot.label,
                                levels = unique(units.long$Plot.label))

# Re-leveling to display in Stats, CS, Domain order
units.long$Area <- factor(units.long$Area, levels = c("Domain", 
                                                      "Computer Science",
                                                      "Statistics/Mathematics"))

# Plot as stacked bar plot
units.plot.bar <- ggplot(data = units.long, mapping = aes(x = Plot.label, 
                                                      y = Proportion,
                                                      fill = Area)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Institution / Program", y = "Proportion of Major") +
  scale_fill_manual(values = c("#D2D2D2", "#222222", "#8F8F8F")) +
  theme_bw()
print(units.plot.bar)

# Plot as parallel coordinates plot

# Figure out rectangles for indicating home department there will need to be 
# three rectangles: cs, math/stats, other. The first two will have some overlap
# as some programs are housed across both cs and math/stats units.
# Rectangles will need to be defined on x-axis at fractional values (i.e. 2.4, 
# 2.6) in order for them to show up correctly
# home.categories <- units.long[, c("Institution", "Home.unit.category", "Plot.label")]
home.categories <- units.long[!duplicated(units.long$Plot.label), "Home.unit.category"]
home.categories <- as.character(home.categories)
cs.indices <- which(home.categories %in% c("cs", "cs; math_stats"))
math.indices <- which(home.categories %in% c("cs; math_stats", "math_stats"))
other.indices <- which(home.categories %in% c("other"))

homes <- data.frame(name = c("Computer\nScience", "Statistics/\nMathematics", "Other"),
                    xmin = c(min(cs.indices), min(math.indices), min(other.indices)) - 0.4,
                    xmax = c(max(cs.indices), max(math.indices), max(other.indices)) + 0.4,
                    fill = c("#1b9e77", "#d95f02", "#7570b3"))
homes$label.x <- (homes$xmin + homes$xmax) / 2

# Re-leveling to display in CS, Stats, Domain order in legend
units.long$Area <- factor(units.long$Area, levels = c("Computer Science",
                                                      "Statistics/Mathematics",
                                                      "Domain"))

# Re-level programs by home unit, then how much CS, then Stats/math, then domain
units.long <- units.long[order(units.long$Home.unit.category, units.long$Proportion, decreasing = c(FALSE, TRUE)), ]
units.long$Plot.label <- factor(units.long$Plot.label,
                                levels = unique(units.long$Plot.label))

units.plot.parallel <- ggplot(data = units.long, mapping = aes(x = Plot.label,
                                                               y = Proportion,
                                                               group = Area,
                                                               fill = Area,
                                                               color = Area)) +
  scale_x_discrete() + # Necessary to get rectangle annotations to work
  annotate(geom = "rect", xmin = homes$xmin, xmax = homes$xmax, ymin = 0, 
           ymax = 1, fill = homes$fill, alpha = 0.2, color = NA) +
  annotate(geom = "text", x = homes$label.x, 
           y = c(0.98, 0.56, 0.98), # dodging math line
           label = homes$name, 
           vjust = 1.0, color = homes$fill) +
  geom_point() +
  geom_line() +
  # Color of the line for Domain is different from the "Other" home category
  scale_color_manual(values = c(as.character(homes$fill[1:2]), "#444444")) +
  theme_bw() +
  xlab(label = "Program") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust = 1.0,
                                   size = 6))
print(units.plot.parallel)
ggsave(filename = "output/program-units.pdf", plot = units.plot.parallel)
