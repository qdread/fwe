# Second year talk figures: optimization plots

ggplot(optimal_df_all %>% filter(category %in% c('GHG','land','water')), aes(x = total_cost, y = cost, color = stage, group = stage)) +
  facet_wrap(~ category) +
  geom_point(size = 2) + geom_line() +
  scale_x_continuous(name = 'Total invested in FLW reduction (million USD)', breaks = c(0, 500, 1000, 2000, 5000)) +
  scale_y_continuous(name = 'Amount invested in each stage (million USD)') +
  scale_color_brewer(type='qual', palette='Set2', guide = guide_legend(nrow = 2)) +
  ggtitle('Optimal allocation of FLW reduction funds to minimize environmental impact', 'for a number of possible total investments') +
  theme_black() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        legend.position = 'bottom')

ggsave(file.path(fpfig, 'secondyeartalk/sixstage_allocations_3impacts.png'), height = 5, width = 9, dpi = 300)

ggplot(optimal_value_all %>% filter(category %in% c('GHG','land','water')), aes(x = total_cost, y = value, color = category)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks = c(0,500,1000,2000,5000), name = 'Total invested (million $)') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), name = 'Impact (percent of baseline)') +
  scale_color_brewer(name = 'Impact category', type = 'qual', palette = 'Set2') +
  ggtitle('Impact reduction by total invested', 'optimizing for each impact category separately') +
  theme_black() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.8, 0.8))

ggsave(file.path(fpfig, 'secondyeartalk/sixstage_allocations_relativetobaseline.png'), height = 5, width = 5, dpi = 300)
