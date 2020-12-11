# Units visualization for undergraduate majors
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-06-07

################################################################################
library(tidyverse)

units_df <- read.csv(file = "data/units.csv")
programs <- read.csv(file = "data/programs.csv")
institutions <- read.csv(file = "data/institution-names.csv")

# Merge the institutions with units to get short names in units_df
units_df <- merge(x = units_df, y = institutions)
rm(institutions)

# Merge units_df with programs
units_df <- merge(x = units_df, y = programs)
rm(programs)

# Limit units df to those programs that are majors (exclude minor & certificate
# programs)
units_df <- units_df[units_df$Major, ]

# Drop any NA rows
units_df <- na.omit(units_df)

# Re-number rows
rownames(units_df) <- NULL

# Calculate midpoint values between min/max to use for plot
# Stats, CS, Domain
units_df$Stats <- (units_df$Stats.min + units_df$Stats.max) / 2
units_df$CS <- (units_df$CS.min + units_df$CS.max) / 2
units_df$Domain <- (units_df$Domain.min + units_df$Domain.max) / 2
units_df$Total <- (units_df$Major.min + units_df$Major.max) / 2

# Calculate values as proportion of total units/hours for major
units_df$Stats.prop <- units_df$Stats / units_df$Total
units_df$CS.prop <- units_df$CS / units_df$Total
units_df$Domain.prop <- units_df$Domain / units_df$Total

# Transform to long format for plotting
units_long <- units_df %>%
  select(Institution, Program, Short.name, Stats.prop, CS.prop, Domain.prop, 
         Program.Abbr, Home.unit.category) %>%
  gather(key = "Area", value = "Proportion", -Institution, -Program, 
         -Short.name, -Program.Abbr, -Home.unit.category)

# Make Area values a little more human readable
units_long$Area <- gsub(pattern = ".prop", replacement = "", units_long$Area)
units_long$Area <- gsub(pattern = "Stats",
                        replacement = "Statistics/Mathematics",
                        units_long$Area)
units_long$Area <- gsub(pattern = "CS",
                        replacement = "Computer Science",
                        units_long$Area)

# Making additional field for plot labels
units_long$Plot.label <- paste0(units_long$Short.name, "\n", 
                                units_long$Program.Abbr)

# Relevel Plot.label so they are ordered by the category of the home department
units_long <- units_long[order(units_long$Home.unit.category, units_long$Institution), ]
units_long$Plot.label <- factor(units_long$Plot.label,
                                levels = unique(units_long$Plot.label))

# Re-leveling to display in Stats, CS, Domain order
units_long$Area <- factor(units_long$Area, levels = c("Domain", 
                                                      "Computer Science",
                                                      "Statistics/Mathematics"))

# Plot as parallel coordinates plot, with shaded rectangles indicating home 
# department/college
home_categories <- units_long[!duplicated(units_long$Plot.label), "Home.unit.category"]
home_categories <- as.character(home_categories)
cs_indices <- which(home_categories %in% c("cs", "cs; math_stats"))
math_indices <- which(home_categories %in% c("cs; math_stats", "math_stats"))
other_indices <- which(home_categories %in% c("other"))

homes <- data.frame(name = c("Computer\nScience", "Statistics/\nMathematics", "Other"),
                    xmin = c(min(cs_indices), min(math_indices), min(other_indices)) - 0.4,
                    xmax = c(max(cs_indices), max(math_indices), max(other_indices)) + 0.4,
                    fill = c("#1b9e77", "#d95f02", "#7570b3"))
homes$label.x <- (homes$xmin + homes$xmax) / 2

# Re-leveling to display in CS, Stats, Domain order in legend
units_long$Area <- factor(units_long$Area, levels = c("Computer Science",
                                                      "Statistics/Mathematics",
                                                      "Domain"))

# Re-level programs by home unit, then how much CS, then Stats/math, then domain
units_long <- units_long[order(units_long$Home.unit.category, units_long$Proportion, decreasing = c(FALSE, TRUE)), ]
units_long$Plot.label <- factor(units_long$Plot.label,
                                levels = unique(units_long$Plot.label))

units_plot_parallel <- ggplot(data = units_long, mapping = aes(x = Plot.label,
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

if (!dir.exists(paths = "output")) {
  dir.create("output")
}

# print(units_plot_parallel)
ggsave(filename = "output/units-visualization.pdf", 
       plot = units_plot_parallel,
       width = 15, height = 10, units = "cm")

ggsave(filename = "output/units-visualization.png",
       plot = units_plot_parallel,
       width = 15, height = 10, units = "cm")
