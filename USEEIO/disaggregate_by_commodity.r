# Disaggregate impacts by commodity (pretty much only for visualization purposes)
# QDR / FWE / 18 Sep 2019

# This uses output from get_lcia_contributions.r

fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')
fp_github <- ifelse(dir.exists('~/Documents/GitHub/'), '~/Documents/GitHub/fwe', '~/fwe')

source(file.path(fp_github, 'USEEIO/load_scenario_data.r'))
source(file.path(fp_github, 'figs/categorylabels.r'))
library(tidyverse)

stage_full_names <- c('production', 'processing', 'retail', 'consumption: foodservice', 'consumption: institutional', 'consumption: household')

stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfoodservice', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')

lcia_contr <- read.csv(file.path(fp_output, 'lcia_contributions.csv'), stringsAsFactors = FALSE, check.names = FALSE)
lcia_contr50 <- read.csv(file.path(fp_output, 'lcia_contributions_best50.csv'), stringsAsFactors = FALSE, check.names = FALSE)

# Load the scenario parameters (which stage or stages reduced) so we can make sure to match everything up properly.
scenpars <- read.csv(file.path(fp_output, 'scenario_parameters_best50.csv'), stringsAsFactors = FALSE)

# Data manipulation -------------------------------------------------------

# Get totals for each stage.

lcia_contr_long <- lcia_contr %>%
  select(-scenario) %>%
  gather(industry, value, -stage_reduced, -impact_category) %>%
  mutate(BEA_389_code = toupper(substr(industry, 1, 6)))

lcia_contr_long50 <- lcia_contr50 %>%
  gather(industry, value, -scenario, -impact_category) %>%
  mutate(BEA_389_code = toupper(substr(industry, 1, 6))) %>%
  mutate(scenario = as.character(scenario))

# Combine the two with scenario parameters
lcia_contr_long <- lcia_contr_long %>%
  rename(scenario = stage_reduced) %>%
  bind_rows(lcia_contr_long50) %>%
  left_join(scenpars)

# Join the codes with the stage they belong to.
lcia_contr_long <- lcia_contr_long %>%
  left_join(naics_foodsystem %>% select(BEA_389_code, stage_code)) %>%
  mutate(stage_code = if_else(is.na(stage_code), 'other', stage_code))

# Sum up
lcia_contr_sums <- lcia_contr_long %>%
  group_by(scenario, impact_category, stage_code) %>%
  summarize(value = sum(value))

# Total contributions of each food type in final demand to impacts

lcia_contr_withprops <- lcia_contr_long %>%
  left_join(naics_foodsystem %>% select(BEA_389_code, cereals:beverages))

lcia_impacts_bycategory <- lcia_contr_withprops %>%
  select(cereals:beverages) %>%
  sweep(1, lcia_contr_withprops$value, `*`)

impacts_byfoodtype <- lcia_contr_withprops %>%
  select(scenario:stage_code) %>%
  cbind(lcia_impacts_bycategory) %>%
  group_by(scenario, impact_category) %>%
  summarize_at(vars(cereals:beverages), sum, na.rm = TRUE)

# We need to split out the total impacts of each commodity in each sector. This might mean running things separately?
total_impacts_byfoodtype <- impacts_byfoodtype %>%
  filter(stage_reduced %in% c('baseline', 'zerowaste'),
         grepl('land|watr|co2|eutr|enrg', impact_category)) %>%
  mutate(fruit_veg = fruit_veg_fresh + fruit_veg_processed,
         fish = fish_fresh + fish_processed,
         roots_tubers = roots_tubers_fresh + roots_tubers_processed) %>%
  select(-contains('fresh'), -contains('processed'))

total_impacts_long <- total_impacts_byfoodtype %>%
  ungroup %>%
  gather(food_type, value, -stage_reduced, -impact_category) %>%
  spread(stage_reduced, value) %>%
  mutate(waste_impact = baseline - zerowaste)