# Create plots of the different impacts from replacing fresh food with canned.

library(dplyr)
library(ggplot2)
library(purrr)

scen_results <- read.csv('Q:/IO_output/twoscenarios.csv', stringsAsFactors = FALSE)
names(scen_results)[1] <- 'impact_category'

scen_results <- scen_results %>%
  mutate(impact_difference = (scenario2 - baseline)/baseline,
         impact_category = map_chr(strsplit(impact_category, '/'), 2))

scen_results %>%
  filter(impact_category %in% c('enrg', 'gcc', 'land', 'watr', 'jobs', 'vadd')) %>%
  mutate(impact_class = case_when(
    impact_category %in% c('jobs', 'vadd') ~ 'economic',
    TRUE ~ 'environmental'
  )) %>%
  mutate(impact_category = factor(impact_category, 
                                  levels = c('enrg', 'gcc', 'land', 'watr', 'jobs', 'vadd'),
                                  labels = c('Energy', 'GHG', 'Land', 'Water', 'Jobs', 'Value-added')),
         impact_class = factor(impact_class,
                               levels = c('environmental', 'economic'))) %>%
  ggplot(aes(x = impact_category, y = impact_difference, fill = impact_class))  + 
  geom_col() +
  facet_grid(. ~ impact_class, scales = 'free_x', space = 'free_x') +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(legend.position = 'none',
        strip.background = element_blank()) +
  labs(x = 'Impact category', y = 'Difference from baseline') +
  ggtitle('50% of fresh fruit/veg converted to canned/frozen')

ggsave('~/Dropbox/projects/foodwaste/Results/diff_baseline_freshtocanned.png', height=5, width=5)

source('read_data/read_lafa.r')

fresh_veg <- veg %>% filter(Category == 'Fresh vegetables', Year == 2015)
canned_veg <- veg %>% filter(Category == 'Canned vegetables', Year == 2015)
frozen_veg <- veg %>% filter(Category == 'Frozen vegetables', Year == 2015)
fresh_fruit <- fruit %>% filter(Category == 'Fresh fruit', Year == 2015)
canned_fruit <- fruit %>% filter(Category == 'Canned fruit', Year == 2015)
frozen_fruit <- fruit %>% filter(Category == 'Frozen fruit', Year == 2015)

# Relative loss of canned+frozen fruit compared to fresh fruit
# Weighted average by calories per day
frozen_fruit$Total_loss__all_levels_Percent
canned_fruit$Total_loss__all_levels_Percent
nonfresh_fruit_loss <- weighted.mean(x = c(frozen_fruit$Total_loss__all_levels_Percent, canned_fruit$Total_loss__all_levels_Percent),
                                     w = c(frozen_fruit$Calories_available_daily_Number, canned_fruit$Calories_available_daily_Number))
fresh_fruit_loss <- fresh_fruit$Total_loss__all_levels_Percent

nonfresh_veg_loss <- weighted.mean(x = c(frozen_veg$Total_loss__all_levels_Percent, canned_veg$Total_loss__all_levels_Percent),
                                   w = c(frozen_veg$Calories_available_daily_Number, canned_veg$Calories_available_daily_Number))
fresh_veg_loss <- fresh_veg$Total_loss__all_levels_Percent




#########################################
# Load by type

# remote vs local file paths
fp <- if (dir.exists('Q:/IO_output')) 'Q:/IO_output' else '~/Dropbox/projects/foodwaste/Data'

scen_results_bytype <- read.csv(file.path(fp, 'twoscenarios_bytype_2012.csv'), stringsAsFactors = FALSE) %>%
  mutate(baseline_total = baseline_freshfruit + baseline_freshveg + baseline_cannedfruitveg,
         scenario2_total = scenario2_freshfruit + scenario2_freshveg + scenario2_cannedfruitveg)
# The totals are the same results as the separate ones.

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

library(reshape2)
library(tidyr)
scen_results_long <- scen_results_bytype %>% 
  melt(id.vars = c('impact_category', 'impact_class'), variable.name = 'food_type', value.name = 'amount') %>%
  separate(food_type, into = c('scenario', 'food_type'))

# Amount of impact for fresh fruit and fresh veg is multiplied by the LAFA loss estimates
# Amount of impact for processed fruit/veg is multiplied by the weighted average of LAFA loss estimate for processed fruit + processed veg
# This is weighted by the original demand amount for fruit and veg



nonfresh_fruitandveg_loss <- weighted.mean(x = c(nonfresh_fruit_loss, nonfresh_veg_loss),
                                           w = orig_demand[1:2])
fresh_fruitandveg_loss <- weighted.mean(x = c(fresh_fruit_loss, fresh_veg_loss),
                                        w = orig_demand[1:2])

loss_data = data.frame(food_type = c('freshfruit', 'freshveg', 'cannedfruitveg'),
                       loss = 0.01 * c(fresh_fruit_loss, fresh_veg_loss, nonfresh_fruitandveg_loss))

scen_results_long <- scen_results_long %>%
  filter(!food_type %in% 'total') %>%
  left_join(loss_data) %>%
  mutate(amount_lost = amount * loss,
         amount_consumed = amount - amount_lost)

ggplot(scen_results_long, aes(x = scenario, y = amount, fill = food_type)) +
  geom_col() +
  facet_wrap(~ impact_category, scales = 'free_y')

scen_results_longer <- scen_results_long %>%
  select(-amount, -loss) %>%
  melt(id.vars = 1:4, variable.name = 'fate', value.name = 'impact_total')

## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

library(RColorBrewer)
color_vec <- c(brewer.pal(3, 'Pastel1'), brewer.pal(3, 'Set1'))

p_stackedwaste <- ggplot(scen_results_longer %>% filter(impact_class == 'environmental'), aes(x = scenario, y = impact_total, fill = interaction(food_type, fate))) +
  geom_col() +
  facet_wrap(~ impact_category, scales = 'free_y') +
  scale_fill_manual(name = 'Fate by food type', values = color_vec, labels = c('Canned/frozen: lost', 'Fresh fruit: lost', 'Fresh veg: lost',
                                                                               'Canned/frozen: consumed', 'Fresh fruit: consumed', 'Fresh veg: consumed')) + 
  theme_bw() +
  theme(strip.background = element_blank()) +
  scale_x_discrete(name = 'Scenario', labels = c('Baseline','Alternative')) +
  scale_y_continuous(name = 'Total impact')

ggsave('~/Dropbox/projects/foodwaste/Results/waste_proportion_by_scenario.png', p_stackedwaste, height = 7, width = 7)

p_difffrombase <- scen_results_bytype %>%
  mutate(impact_difference = (scenario2_total - baseline_total)/baseline_total) %>%
  ggplot(aes(x = impact_category, y = impact_difference, fill = impact_class))  + 
  geom_col() +
  facet_grid(. ~ impact_class, scales = 'free_x', space = 'free_x') +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(legend.position = 'none',
        strip.background = element_blank()) +
  labs(x = 'Impact category', y = 'Difference from baseline') +
  scale_y_continuous(labels = scales::percent) +
  ggtitle('50% of fresh fruit/veg converted to canned/frozen')

ggsave('~/Dropbox/projects/foodwaste/Results/diff_baseline_freshtocanned.png', p_difffrombase, height=5, width=5)


# Plot the waste amounts only

p_stackedwasteonly <- ggplot(scen_results_longer %>% filter(impact_class == 'environmental', fate == 'amount_lost'), aes(x = scenario, y = impact_total, fill = food_type)) +
  geom_col() +
  facet_wrap(~ impact_category, scales = 'free_y') +
  scale_fill_manual(name = 'Food type', values = color_vec[1:3], labels = c('Canned/frozen fruit+veg', 'Fresh fruit', 'Fresh veg')) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  scale_x_discrete(name = 'Scenario', labels = c('Baseline','Alternative')) +
  scale_y_continuous(name = 'Total impact')

ggsave('~/Dropbox/projects/foodwaste/Results/wasteonly_by_scenario.png', p_stackedwasteonly, height = 7, width = 7)
