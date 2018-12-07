# Plot scenarios that changed the structure of the model
# Change relative to baseline

library(dplyr)
library(ggplot2)
library(purrr)

fp <- if (dir.exists('Q:/IO_output')) 'Q:/IO_output' else '~/Dropbox/projects/foodwaste/Data'

scen_results_bytype <- read.csv(file.path(fp, 'structural_scenarios_2012.csv'), stringsAsFactors = FALSE)


scen_results_bytype <- scen_results_bytype %>%
  rename(impact_category = X) %>%
  mutate(impact_category = map_chr(strsplit(impact_category, '/'), 2)) %>%
  filter(impact_category %in% c('enrg', 'gcc', 'land', 'watr', 'jobs', 'vadd')) %>%
  mutate(impact_class = case_when(
    impact_category %in% c('jobs', 'vadd') ~ 'economic',
    TRUE ~ 'environmental'
  )) %>%
  mutate(impact_category = factor(impact_category, 
                                  levels = c('enrg', 'gcc', 'land', 'watr', 'jobs', 'vadd'),
                                  labels = c('Energy', 'GHG', 'Land', 'Water', 'Jobs', 'Value-added')),
         impact_class = factor(impact_class,
                               levels = c('environmental', 'economic')))

scen_results_bytype <- scen_results_bytype %>%
  mutate(result1diff = result1 - result0, result2diff = result2 - result0, result3diff = result3 - result0)

#### change this to log ratio or log fold change.

# Plot differences from the baseline.

library(reshape2)
library(tidyr)
scen_results_long <- scen_results_bytype %>% 
  melt(id.vars = c('impact_category', 'impact_class'), variable.name = 'food_type', value.name = 'amount') %>%
  separate(food_type, into = c('scenario', 'food_type'))

scen_results_long <- scen_results_bytype %>% 
  select(-(result0:result3)) %>%
  gather(scenario, amount, -c(impact_category, impact_class))
  
p_difffrombase <- scen_results_long %>%
  filter(impact_class == 'environmental') %>%
  ggplot(aes(x = scenario, y = amount, fill = scenario))  + 
  geom_col() +
  facet_wrap(~ impact_category, scales = 'free_y', labeller = labeller(impact_category = c('Energy' = 'Energy (MJ)', 'GHG' = 'GHG (kg CO2 eq.)', 'Land' = 'Land (m2/y)', 'Water' = 'Water (m3)'))) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(legend.position = 'none',
        strip.background = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = 'Difference from baseline') +
  scale_x_discrete(labels = paste('Scenario', 1:3)) +
  ggtitle('Impacts relative to baseline for some scenarios')



