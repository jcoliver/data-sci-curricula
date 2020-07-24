#' Generate caterpillar-style plot of area scores
#' 
#' @param framework        character vector indicating which framework to plot
#' @param plot_height_mod  integer height modifier for resultant output file
catplot <- function(framework = c("na", "gds"), plot_height_mod = 1) {
  if (!require(tidyverse)) {
    stop("catplot requires tidyverse package, which could not be loaded.")
  }
  
  score_file <- paste0("data/", framework, "-scores.csv")
  scores <- read.csv(file = score_file, stringsAsFactors = FALSE)
  areas_file <- paste0("data/", framework, "-areas-key.csv")
  areas <- read.csv(file = areas_file, stringsAsFactors = FALSE)
  inst.names <- read.csv(file = "data/institution-names.csv", stringsAsFactors = FALSE)
  
  # Add also institutional data, which has value of Short.name
  scores <- merge(x = scores, y = inst.names)
  
  # Convert scores to long
  scores_long <- scores %>%
    pivot_longer(names_to = "subarea",
                 values_to = "score",
                 cols = -c(Institution, Program, Short.name),
                 values_drop_na = TRUE)
  
  # Merge with the area data frame so we can line up subareas with appropriate 
  # area
  scores_long <- merge(x = scores_long,
                       y = areas[, c("Area", "Key")],
                       by.x = "subarea",
                       by.y = "Key")
  
  # Renaming a column because I'm being pedantic
  scores_long <- scores_long %>%
    rename(area = Area)
  
  # For long-formatted data, want to re-level area, from highest mean score to 
  # lowest mean score
  
  # Start by calculating summary statistics
  options(dplyr.summarise.inform = FALSE)
  area_stats <- scores_long %>%
    group_by(area) %>%
    summarize(mean_score = mean(score),
              sd_score = sd(score),
              se_score = sd(score)/sqrt(n()),
              median_score = median(score),
              first_quartile = quantile(x = score, probs = 0.25),
              third_quartile = quantile(x = score, probs = 0.75))
  options(dplyr.summarise.inform = TRUE)

  # Re-level area based on mean values
  area_levels <- area_stats$area[order(area_stats$mean_score)]
  area_stats$area <- factor(x = area_stats$area,
                            levels = area_levels)
  
  # Write the summary stats to a file while we are here
  write.csv(x = area_stats, 
            file = paste0("output/table-scores-", framework, ".csv"),
            row.names = FALSE)
  
  # Create means for each program X institution combination so we can add points 
  # for each program to plot
  options(dplyr.summarise.inform = FALSE)
  program_means <- scores_long %>%
    group_by(Short.name, Program, area) %>%
    summarize(score = mean(score))
  options(dplyr.summarise.inform = TRUE)
  
  # Will want to get labels for the graph, but only need it for Areas (not sub-
  # areas)
  plot_labels <- areas %>%
    filter(Status == "Area") %>%
    select(Area, Label) %>%
    rename(area = Area,
           label = Label)
  
  # Update description with ampersands and linebreaks to conserve horizontal 
  # space in plot
  plot_labels$label <- stringr::str_wrap(string = plot_labels$label, 
                                         width = 20)
  
  # And order the data frame to align with levels based on mean scores
  plot_labels$area <- factor(x = plot_labels$area,
                             levels = area_levels)
  plot_labels <- plot_labels[order(plot_labels$area), ]
  # And finally re-level label, which will dictate the order in which 
  # things appear on the axis
  plot_labels$label <- factor(x = plot_labels$label,
                              levels = plot_labels$label)
  
  scores_catplot <- ggplot(data = area_stats, mapping = aes(x = area)) +
    geom_errorbar(mapping = aes(ymin = mean_score - (2 * se_score),
                                ymax = mean_score + (2 * se_score)),
                  width = 0.0) +
    geom_point(mapping = aes(y = mean_score), 
               size = 2.5, 
               shape = 21,
               fill = "white") +
    scale_x_discrete(name = "Area",
                     limits = plot_labels$area,
                     labels = plot_labels$label) +
    geom_point(data = program_means,
               mapping = aes(x = area, y = score),
               shape = 21,
               fill = "black",
               alpha = 0.25,
               size = 1)
  
  # If they are available, get the letters from post-hoc tests to add to plot
  letters_file <- paste0("output/", framework, "-lme-letters.csv")
  
  if (file.exists(letters_file)) {
    posthoc_letters <- read.csv(file = letters_file, stringsAsFactors = FALSE)
    
    # If letters are going to be used for plotting, they should probably be 
    # separated by commas. A base R way of doing this would be 
    plot_letters <- unlist(lapply(X = strsplit(x = posthoc_letters$Letters, split = ""),
                                  FUN = function(x) {
                                    paste(x, collapse = ", ")
                                  }))
    posthoc_letters$Plot_letters <- plot_letters
    scores_catplot <- scores_catplot +
      annotate("text", 
               x = posthoc_letters$Area, 
               y = 0.5, 
               label = posthoc_letters$Plot_letters,
               adj = 0,
               size = 3.5) +
      ylim(c(0.5, 4)) # Need to add space if we add letters
    
  } else {
    message("Could not find file for adding post-hoc letters to plot")
  }
  
  # Some final theming for the figure
  scores_catplot <- scores_catplot + 
    labs(x = "Area",
         y = "Score") +
    theme_bw() +
    coord_flip()
  scores_catplot
  
  ggsave(filename = paste0("output/figure-scores-catplot-", framework, ".pdf"),
         plot = scores_catplot,
         width = 10,
         height = nrow(plot_labels) + plot_height_mod, 
         units = "cm")
  
}