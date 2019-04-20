# Create plots of updated scenario results
# QDR / FWE / 19 Apr 2019

library(tidyverse)

fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
eeio_result <- read.csv(file.path(fp_output, 'fao_scenario_lcia_results.csv'), stringsAsFactors = FALSE)

ggplot(eeio_result %>% filter(grepl('gcc|land|watr|eutr', impact_category)), aes(x = scenario, y = value)) +
  geom_point(size = 2) +
  facet_wrap(~ impact_category, scales = 'free_y') +
  theme_bw() +
  theme(text = element_text(size = 14))

# Convert units to smaller numbers, and normalize everything off the baseline.

unit_list <- c('kg', 'kg', 'kg', 'n', '$', 'kg', 'ctu', 'kg', 'kg', 'ctu', 'ctu', 'kg', 'ctu', 'kg', 'MJ', 'm2', 'kg', 'MJ', 'MJ', 'm3')
conversion_factor <- c(1e-6, 1e-6, 1e-6, 1e-6, 1e-9, 1e-9, 1e-12, 1e-9, 1e-9, 1, 1, 1e-9, 1, 1e-3, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9)

eeio_result_editunits <- eeio_result %>%
  spread(scenario, value) %>%
  mutate_if(is.numeric, ~ .x * conversion_factor) %>%
  gather(scenario, value, -impact_category)
  
# Long names for y-axes, with units now in more reasonable ranges of numbers
category_labels <- list('Hazardous air pollutants released (kT)',
                        'Heavy metals released (kT)',
                        'Pesticides released (kT)',
                        'Millions of jobs created',
                        'Value added (billion $)',
                        expression(paste('Acid released (MT SO'[2], ' eq.)')),
                        expression(paste('Environmental toxicity (10'^12, ' CTUe)')),
                        'Eutrophication (MT N eq.)',
                        expression(paste('GHG emissions (MT CO'[2], ' eq.)')),
                        'Human toxicity, cancer (CTUh)',
                        'Human toxicity, non-cancer (CTUh)',
                        expression(paste('HRSP (MT PM'[2.5], ' eq.)')),
                        'Ozone depletion (MT CFC-11 eq.)',
                        expression(paste('Smog (kg O'[3], 'eq.)')),
                        )

# Create long names for y-axes
category_labels <- list(expression(paste('Acid released (kg SO'[2], ' eq.)')),
                        'Energy used (MJ)',
                        'Environmental toxicity (CTUe)',
                        'Eutrophication (kg N eq.)',
                        expression(paste('GHG emissions (kg CO'[2], ' eq.)')),
                        'HAPS released (kg)',
                        'Human toxicity, cancer (CTUh)',
                        'Human toxicity, non-cancer (CTUh)',
                        expression(paste('HRSP (kg PM'[2.5], ' eq.)')),
                        'HTOX (CTUh)',
                        'Jobs created',
                        expression(paste('Land used (m'^2, ' y'^-1,')', sep='')),
                        'Heavy metals released (kg)',
                        'Minerals used (kg)',
                        'NREN (MJ)', # Nonrenewable energy (component of energy used)
                        'Ozone depletion (kg CFC eq.)',
                        'Pesticide released (kg)',
                        'REN (MJ)', # Renewable energy (component of energy used)
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