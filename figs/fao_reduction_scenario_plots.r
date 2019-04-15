# Run USEEIO model with demand scenarios for food system created by taking FAO loss rates as baseline percentages
# Create plots of a few results
# QDR / FWE / 15 April 2019

library(tidyverse)
library(reticulate)

source_python('~/Documents/GitHub/fwe/USEEIO/eeio_lcia.py')

all_codes <- read.csv('~/Dropbox/projects/foodwaste/Data/all_codes.csv', stringsAsFactors = FALSE)
demand <- read.csv('~/Dropbox/projects/foodwaste/Data/demand_scenarios_2012_wastereductionbystage.csv', stringsAsFactors = FALSE)

# Get the correct codes with full description names matched up with the demand vectors.
demand <- demand %>%
  left_join(all_codes, by = c('BEA_389_code' = 'sector_code_uppercase'))

# Model is already built under the name USEEIO2012

# Convert nonzero elements of demand vector to separate lists of codes and values

demand_codes <- as.list(demand$sector_desc_drc[demand$demand_baseline > 0])
demand_values <- demand %>%
  select(starts_with('demand')) %>%
  map(~ as.list(.x[.x > 0]))

eeio_result <- map(demand_values, ~ eeio_lcia('USEEIO2012', .x, demand_codes))
impact_categories <- rownames(eeio_result[[1]])
eeio_result <- imap_dfr(eeio_result, ~ data.frame(scenario = .y, .x))
eeio_result <- data.frame(scenario = map_chr(strsplit(eeio_result$scenario, '_'), 2),
                          impact_category = impact_categories,
                          value = eeio_result$Total,
                          stringsAsFactors = FALSE)

ggplot(eeio_result %>% filter(grepl('gcc|land|watr|eutr', impact_category)), aes(x = scenario, y = value)) +
  geom_point(size = 2) +
  facet_wrap(~ impact_category, scales = 'free_y') +
  theme_bw() +
  theme(text = element_text(size = 14))

# Create long names for y-axes
category_labels <- list(expression(paste('Acid released (kg SO'[2], ' eq.)')),
                     'Energy used (MJ)',
                     'Environmental toxicity (CTUE)',
                     'Eutrophication (kg N eq.)',
                     expression(paste('GHG emissions (kg CO'[2], ' eq.)')),
                     'HAPS released (kg)',
                     'HC (CTUH)',
                     'HNC (CTUH)',
                     expression(paste('HRSP (kg PM'[2.5], ' eq.)')),
                     'HTOX (CTUH)',
                     'Jobs created',
                     expression(paste('Land used (m'^2, ' y'^-1,')', sep='')),
                     'Heavy metals released (kg)',
                     'Resources mined (kg)',
                     'NREN (MJ)',
                     'Ozone depletion (kg CFC eq.)',
                     'Pesticide released (kg)',
                     'REN (MJ)',
                     expression(paste('Smog (kg O'[3], 'eq.)')),
                     'Value added ($)',
                     expression(paste('Water used (m'^3, ')', sep = ''))
)

# Separate plot for each category
plots_by_category <- eeio_result %>%
  group_by(impact_category) %>%
  do(p = ggplot(., aes(x = scenario, y = value, color = scenario)) +
    geom_point(size = 4) +
    scale_x_discrete(name = 'Scenario', labels = c('Baseline', '25%\nProducer', '25%\nRetailer', '25%\nConsumer')) +
    scale_color_brewer(type = 'qual', palette = 'Set2') +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), legend.position = 'none',
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14)))

labeled_plots <- map2(impact_categories, category_labels, ~ plots_by_category$p[[which(plots_by_category$impact_category == .x)]] + scale_y_continuous(name = .y))

pdf('~/google_drive/SESYNC Food Waste/Results_Figures/three_reduction_scenarios.pdf', height = 4, width = 4.5)
  walk(labeled_plots, print)
dev.off()

# Create black and white theme
source('~/Documents/R/theme_black.R')
th <- theme_black() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), legend.position = 'none',
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14))
ggsave('~/Dropbox/sesync/firstyear_talk/scenarioresult_GHG.png', labeled_plots[[5]] + th, height = 4, width = 5, dpi = 300)
ggsave('~/Dropbox/sesync/firstyear_talk/scenarioresult_land.png', labeled_plots[[12]] + th, height = 4, width = 5, dpi = 300)
ggsave('~/Dropbox/sesync/firstyear_talk/scenarioresult_water.png', labeled_plots[[21]] + th, height = 4, width = 5, dpi = 300)
