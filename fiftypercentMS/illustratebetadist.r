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

# Show all the beta distributions, colored by stage.

# Combine the two columns into one for L2.

lines_all <- pcts %>%
  select(-category_number) %>%
  mutate(loss_processing_packaging = 1 - (1 - loss_handling_storage) * (1 - loss_processing_packaging)) %>%
  select(- loss_handling_storage) %>%
  pivot_longer(-category, names_to = 'stage') %>%
  mutate(stage = factor(stage, levels = c('loss_ag_production', 'loss_processing_packaging', 'loss_distribution', 'loss_consumption'),
                        labels = c('production','processing','retail','consumption'))) %>%
  group_by(category, stage) %>%
  group_modify(~ data.frame(x = xGrid, y = dbeta(xGrid, .$value * 100, 100 - (.$value * 100)))) 

allbeta <- ggplot(lines_all, aes(x=x, y=y, color=category)) +
  facet_wrap(~ stage) +
  geom_line(size = 1) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = 'bottom', strip.background = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(name = 'Loss rate', expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(12, 'Paired'), 'black'), guide = guide_legend(nrow = 5))

ggsave('Q:/figures/stoten_ms/all_beta.png', allbeta, height = 8, width = 6.5, dpi = 300)
