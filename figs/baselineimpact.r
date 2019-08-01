# Per capita baseline food system and food waste impacts
# Compare the 0% reduction with 100% reduction across all stages

library(tidyverse)
fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')

stage_full_names <- c('production', 'processing', 'retail', 'consumption: food service', 'consumption: institutional', 'consumption: household')

stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfood service', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')

base_and_nowaste <- read.csv(file.path(fp_output, 'sixstage_scenario_grid_lcia_results.csv'), stringsAsFactors = FALSE) %>%
  filter(rowSums(.[,1:6]) %in% c(0, 6))

waste_impacts <- base_and_nowaste %>%
  mutate(scenario = if_else(rowSums(.[,1:6]) == 0, 'baseline', 'zero_waste')) %>%
  select(scenario, impact_category, value) %>%
  spread(scenario, value) %>%
  mutate(waste_impact = baseline - zero_waste,
         proportion_impact = waste_impact / baseline)

# US population in 2012 was 314 million
pop <- 314e6

waste_impacts <- mutate(waste_impacts, percapita_waste_impact = waste_impact / pop)
write.csv(waste_impacts, file = file.path(fp_output, 'baseline_impacts.csv'), row.names = FALSE)

# 71000 L water, 1790 m2 land, 5.5 GJ energy
# 539 kg CO2 eq, 1.7 kg N eq
# 17% of water used by food system is "wasted", as is 16% of land and 16% of energy.

