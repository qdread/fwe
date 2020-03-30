# Compare our results to ReFED's results for the same interventions
# QDR / FWE / 30 March 2020

library(tidyverse)
library(readxl)
library(units)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')

refed <- read_xlsx(file.path(fp, 'scenario_inputdata/ReFED-Data-Set.xlsx'), .name_repair = 'universal') %>%
  setNames(gsub('[\\.]+', '_', names(.)))

# Match intervention names in refed.
refed <- refed %>%
  mutate(intervention = gsub('&', 'and', Solution) %>% tolower)


# Compare impacts averted -------------------------------------------------


# Compare GHG emissions averted. Use only the total for waste tracking
all_ghg <- all_interventions_4cat %>%
  filter(grepl('greenhouse',category), !grepl('waste tracking', intervention) | scenario == 'total')

all_ghg %>% 
  left_join(refed) %>%
  ggplot(aes(x = GHGs_K_tons_year_/1000, y = net_averted_mean)) +
    geom_point()

all_ghg_table <- all_ghg %>% 
  left_join(refed) %>%
  select(intervention, GHGs_K_tons_year_, net_averted_mean, net_averted_lower, net_averted_upper) %>%
  setNames(c('intervention', 'ReFED_estimate', 'our_estimate', 'our_lower_bound', 'our_upper_bound')) %>%
  mutate(ReFED_estimate = ReFED_estimate / 1000)

# Compare water saved in the same way.
all_h2o <- all_interventions_4cat %>%
  filter(grepl('water',category), !grepl('waste tracking', intervention) | scenario == 'total')

# km^3 for our estimate, billion gallons for refed, must convert
all_h2o_table <- all_h2o %>% 
  left_join(refed) %>%
  select(intervention, Water_Conservation_B_gals_yr_, net_averted_mean, net_averted_lower, net_averted_upper) %>%
  setNames(c('intervention', 'ReFED_estimate', 'our_estimate', 'our_lower_bound', 'our_upper_bound')) %>%
  mutate(ReFED_estimate = ReFED_estimate * 1e9 %>% set_units(gallon) %>% set_units(km^3))



# Compare costs -----------------------------------------------------------

# Compare total cost
# for refed this is Cost_M_year_

all_cost_table <- dat_totalcost %>% 
  filter(!grepl('waste tracking', intervention) | scenario == 'total') %>%
  left_join(refed) %>%
  select(intervention, Cost_M_year_, mean, lower, upper) %>%
  setNames(c('intervention', 'ReFED_estimate', 'our_estimate', 'our_lower_bound', 'our_upper_bound')) %>%
  mutate(ReFED_estimate = -ReFED_estimate) %>%
  mutate_at(vars(starts_with('our')), ~ . / 1e6)
  

# Compare cost effectiveness
# for refed, divide cost by the tons of ghg or water saved.


refed_costeff <- refed %>%
  select(intervention, Cost_M_year_, GHGs_K_tons_year_, Water_Conservation_B_gals_yr_) %>%
  setNames(c('intervention','annual_cost', 'ghg', 'water')) %>%
  mutate(water = water * 1e9 %>% set_units(gallon) %>% set_units(m^3),
         ghg = ghg %>% set_units(kilotonne) %>% set_units(kg),
         costeff_ghg = -1e-6 * ghg/annual_cost,
         costeff_water = -1e-6 * water/annual_cost) 
  
# GHG cost effectiveness
costeff_ghg_table <- all_interventions_4cat %>%
  filter(grepl('greenhouse', category)) %>%
  select(intervention, scenario, starts_with('cost_per')) %>%
  mutate_at(vars(starts_with('cost_per')), ~ 1 / .) %>%
  left_join(refed_costeff %>% select(intervention, costeff_ghg)) %>%
  select(intervention, scenario, costeff_ghg, cost_per_reduction_mean, cost_per_reduction_lower, cost_per_reduction_upper) %>%
  setNames(c('intervention','scenario','ReFED_estimate','our_estimate','our_lower_bound', 'our_upper_bound'))
  
# Water cost effectiveness
costeff_water_table <- all_interventions_4cat %>%
  filter(grepl('water', category)) %>%
  select(intervention, scenario, starts_with('cost_per')) %>%
  mutate_at(vars(starts_with('cost_per')), ~ 1 / .) %>%
  left_join(refed_costeff %>% select(intervention, costeff_water)) %>%
  select(intervention, scenario, costeff_water, cost_per_reduction_mean, cost_per_reduction_upper, cost_per_reduction_lower) %>%
  setNames(c('intervention','scenario','ReFED_estimate','our_estimate','our_lower_bound', 'our_upper_bound'))


# Make plots --------------------------------------------------------------

# Combine all the data into a single data frame
# Not needed
plot_dat <- map2_dfr(list(all_ghg_table, all_h2o_table, all_cost_table, costeff_ghg_table, costeff_water_table),
                     c('GHG impact averted', 'water impact averted', 'total annual cost', 'GHG cost-effectiveness', 'water cost-effectiveness'),
                     ~ data.frame(name = .y, .x))

theme_set(theme_bw() +
            theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position = 'none'))

p_ghgaverted <- ggplot(all_ghg_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = ReFED_estimate), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_pointrange(aes(y = our_estimate, ymin = our_lower_bound, ymax = our_upper_bound)) +
  scale_y_continuous(name = parse(text = 'Potential~GHG~emissions~reduced~(MT~CO[2])')) +
  coord_flip() 

p_wateraverted <- ggplot(all_h2o_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = as.numeric(ReFED_estimate)), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_pointrange(aes(y = our_estimate, ymin = our_lower_bound, ymax = our_upper_bound)) +
  scale_y_continuous(name = parse(text = 'Potential~water~use~reduced~(km^3)')) +
  coord_flip() 

p_totalcost <- ggplot(all_cost_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = ReFED_estimate), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_pointrange(aes(y = our_estimate, ymin = our_lower_bound, ymax = our_upper_bound)) +
  scale_y_continuous(name = 'Total annual cost (million $)') +
  coord_flip() 

p_ghgcosteff <- ggplot(costeff_ghg_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(data = costeff_ghg_table %>% filter(is.na(scenario) | scenario == 'total') %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(y = as.numeric(ReFED_estimate)), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_pointrange(aes(x = intervention, group = scenario, y = our_estimate, ymin = our_lower_bound, ymax = our_upper_bound), position = position_dodge(width = 0.2)) +
  annotate(geom = 'text', x = c(4.2, 3.8), y = c(15, 60), label = c('restaurants', 'contractors'), size = 3) +
  scale_y_continuous(name = parse(text = 'GHG~cost-effectiveness~("$"/kg~CO[2])')) +
  coord_flip() 

p_watercosteff <- ggplot(costeff_water_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(data = costeff_water_table %>% filter(is.na(scenario) | scenario == 'total') %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(y = as.numeric(ReFED_estimate)), pch = 1, alpha = 0.5, size = 4, color = 'black') +
  geom_pointrange(aes(x = intervention, group = scenario, y = our_estimate, ymin = our_lower_bound, ymax = our_upper_bound), position = position_dodge(width = 0.2)) +
  scale_y_continuous(name = parse(text = 'water~use~cost-effectiveness~("$"/m^3)')) +
  coord_flip() 


# Save plots --------------------------------------------------------------

fp_fig <- file.path(fp, 'figures/intervention_analysis')

ggsave(file.path(fp_fig, 'four_interventions_vsrefed_ghg_reduced.png'), p_ghgaverted, height = 4, width = 5, dpi = 300)
ggsave(file.path(fp_fig, 'four_interventions_vsrefed_water_reduced.png'), p_wateraverted, height = 4, width = 5, dpi = 300)
ggsave(file.path(fp_fig, 'four_interventions_vsrefed_total_annual_cost.png'), p_totalcost, height = 4, width = 5, dpi = 300)
ggsave(file.path(fp_fig, 'four_interventions_vsrefed_ghg_costeff.png'), p_ghgcosteff, height = 4, width = 5, dpi = 300)
ggsave(file.path(fp_fig, 'four_interventions_vsrefed_water_costeff.png'), p_watercosteff, height = 4, width = 5, dpi = 300)
