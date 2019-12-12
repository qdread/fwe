# Illustration of the beta distributions used

library(ggplot2)
library(tidyverse)

cols <- RColorBrewer::brewer.pal(8, 'Set3')[-2]

# Use percentage values for fruit and veg, and for meat.
pcts <- read.csv('Q:/crossreference_tables/fao_percentages_extended.csv')

pct_plot <- pcts %>%
  filter(category %in% c('fruit and vegetable, fresh', 'meat')) %>%
  select(-category_number) %>%
  pivot_longer(-category)

# evaluate pct_plot for each beta distribution at a grid
xGrid <- seq(0.001, 0.5, by = 0.001)

lines_plot <- pct_plot %>% 
  group_by(category, name) %>%
  group_modify(~ data.frame(x = xGrid, y = dbeta(xGrid, .$value * 100, 100 - (.$value * 100))))

ggplot(lines_plot, aes(x=x, y=y, group=interaction(category,name), color=category)) +
  geom_line(size = 1) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 50))

