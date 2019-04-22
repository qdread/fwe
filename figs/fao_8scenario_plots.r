# Create plots of updated scenario results
# QDR / FWE / 19 Apr 2019

library(tidyverse)

fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
fp_fig <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'figures')
eeio_result <- read.csv(file.path(fp_output, 'fao_scenario_lcia_results.csv'), stringsAsFactors = FALSE)

ggplot(eeio_result %>% filter(grepl('gcc|land|watr|eutr', impact_category)), aes(x = scenario, y = value)) +
  geom_point(size = 2) +
  facet_wrap(~ impact_category, scales = 'free_y') +
  theme_bw() +
  theme(text = element_text(size = 14))

# Convert units to smaller numbers, and normalize everything off the baseline.

unit_list <- c('kg', 'kg', 'kg', 'n', '$', 'kg', 'ctu', 'kg', 'kg', 'ctu', 'ctu', 'kg', 'ctu', 'kg', 'MJ', 'm2', 'kg', 'MJ', 'MJ', 'm3')
conversion_factor <- c(1e-6, 1e-6, 1e-6, 1e-6, 1e-9, 1e-9, 1e-12, 1e-9, 1e-9, 1, 1, 1e-9, 1, 1e-3, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9)

levels_ordered <- c('baseline', 'L1', 'L2', 'L3', 'L1L2', 'L1L3', 'L2L3' ,'L1L2L3')
labels_ordered <- c('Baseline', 'Producer', 'Retailer', 'Consumer', 'P+R', 'P+C', 'R+C', 'P+R+C')

eeio_result_editunits <- eeio_result %>%
  spread(scenario, value) %>%
  mutate_if(is.numeric, ~ .x * conversion_factor) %>%
  gather(scenario, value, -impact_category) %>%
  mutate(scenario = factor(scenario, levels = levels_ordered, labels = labels_ordered))
  
eeio_result_normalized <- eeio_result %>%
  spread(scenario, value) %>%
  mutate_if(is.numeric, `/`, .$baseline) %>%
  gather(scenario, value, -impact_category) %>%
  mutate(scenario = factor(scenario, levels = levels_ordered, labels = labels_ordered))


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
                        expression(paste('Particulates released (MT PM'[2.5], ' eq.)')),
                        'Human toxicity, total (CTUh)',
                        'Ozone depletion (MT CFC-11 eq.)',
                        expression(paste('Smog (kg O'[3], ' eq.)')),
                        'Total energy used (PJ)',
                        expression(paste('Land used (thousand km'^2, ' y'^-1,')', sep='')),
                        'Minerals used (MT)',
                        'Nonrenewable energy used (PJ)',
                        'Renewable energy used (PJ)',
                        expression(paste('Water used (km'^3, ')', sep = ''))
                        )

category_labels_nounits <- c('Hazardous air pollutants released',
                             'Heavy metals released',
                             'Pesticides released',
                             'Millions of jobs created',
                             'Value added',
                             'Acid released',
                             'Environmental toxicity',
                             'Eutrophication potential',
                             'GHG emissions',
                             'Human toxicity, cancer',
                             'Human toxicity, non-cancer',
                             'Particulates released',
                             'Human toxicity, total',
                             'Ozone depletion',
                             'Smog',
                             'Total energy used',
                             'Land used',
                             'Minerals used',
                             'Nonrenewable energy used',
                             'Renewable energy used',
                             'Water used'
)


# Separate plot for each category
plots_by_category <- eeio_result_editunits %>%
  group_by(impact_category) %>%
  do(p = ggplot(., aes(x = scenario, y = value, color = scenario)) +
       geom_hline(data = . %>% filter(scenario %in% 'Baseline'), aes(yintercept = value), linetype = 'dotted', size = 1) +
       geom_point(data = . %>% filter(!scenario %in% 'Baseline'), size = 4) +
       scale_x_discrete(name = 'Scenario') +
       scale_color_brewer(type = 'qual', palette = 'Set2') +
       theme_bw() +
       theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), legend.position = 'none',
             axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
             axis.text.x = element_text(size = 14)))

labeled_plots <- plots_by_category %>%
  ungroup %>%
  mutate(category_label = category_labels) %>%
  rowwise %>%
  do(p = .$p + scale_y_continuous(name = .$category_label))

pdf(file.path(fp_fig, 'fao_8_reduction_scenarios.pdf'), height = 5, width = 6.5)
  walk(labeled_plots$p, print)
dev.off()

# Separate plot for each category, with normalized units
plots_by_category_percentage <- eeio_result_normalized %>%
  group_by(impact_category) %>%
  do(p = ggplot(., aes(x = scenario, y = value, color = scenario)) +
       geom_hline(data = . %>% filter(scenario %in% 'Baseline'), aes(yintercept = value), linetype = 'dotted', size = 1) +
       geom_point(data = . %>% filter(!scenario %in% 'Baseline'), size = 4) +
       scale_x_discrete(name = 'Scenario') +
       scale_color_brewer(type = 'qual', palette = 'Set2') +
       theme_bw() +
       theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), legend.position = 'none',
             axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
             axis.text.x = element_text(size = 14)))

labeled_plots_percentage <- plots_by_category_percentage %>%
  ungroup %>%
  mutate(category_label = category_labels_nounits) %>%
  rowwise %>%
  do(p = .$p + scale_y_continuous(name = .$category_label, limits = c(0.95, 1), labels = scales::percent))

pdf(file.path(fp_fig, 'fao_8_reduction_scenarios_percentages.pdf'), height = 5, width = 6.5)
  walk(labeled_plots_percentage$p, print)
dev.off()

# Radar plot to visualize several relevant categories at once
library(ggradar)

# the radar plot requires "wide" data
radardat <- eeio_result_normalized %>%
  spread(impact_category, value)


radardat <- eeio_result %>%
  spread(scenario, value) %>%
  mutate_if(is.numeric, `/`, .$baseline) %>%
  cbind(category_label = category_labels_nounits) %>%
  select(-impact_category) %>%
  gather(scenario, value, -category_label) %>%
  mutate(scenario = factor(scenario, levels = levels_ordered, labels = labels_ordered),
         value = 1 - value) %>%
  spread(category_label, value) 

radardat_reduced <- radardat %>%
  filter(!scenario %in% 'Baseline') %>%
  select(scenario, `GHG emissions`, `Water used`, `Land used`, `Total energy used`, `Eutrophication potential`) 

radar1 <- ggradar(radardat_reduced %>% filter(scenario %in% c('Consumer','Producer','Retailer')) %>% rename(Eutrophication = `Eutrophication potential`), 
                  grid.max = 0.03, label.gridline.max = FALSE, label.gridline.min = FALSE, axis.label.size = 2.7) +
  ggtitle('Magnitude of impact reduction by waste reduction scenario', 'All waste reduction at a single stage') +
  theme(title = element_text(size = 8), legend.position = 'bottom')

radar2 <- ggradar(radardat_reduced %>% filter(scenario %in% c('P+R', 'P+C', 'R+C', 'P+R+C')) %>% rename(Eutrophication = `Eutrophication potential`), 
                  grid.max = 0.03, label.gridline.max = FALSE, label.gridline.min = FALSE, axis.label.size = 2.7) +
  ggtitle('Magnitude of impact reduction by waste reduction scenario', 'Waste reduction evenly divided across multiple stages') +
  theme(title = element_text(size = 8), legend.position = 'bottom')

ggsave(file.path(fp_fig, 'fao_radarplot_1stage.png'), radar1,  height = 5, width = 7, dpi = 300)
ggsave(file.path(fp_fig, 'fao_radarplot_2and3stages.png'), radar2,  height = 5, width = 7, dpi = 300)
