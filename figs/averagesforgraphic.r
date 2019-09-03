# Roughly calculate the average GHG intensity per $1 for each of the sectors in USEEIO model
# Manually extract the CO2eq values for the different GHGs, then multiply out by the flows
# Average them for each FSC stage.

library(XLConnect)
library(tidyverse)

ghgwkbk <- loadWorkbook('~/Documents/GitHub/USEEIO/useeiopy/USEEIOv1.1 Satellite Excel Files/USEEIOv1.1_Satellite_GHG.xlsx')
flowharmonization <- readWorksheet(ghgwkbk, 'FlowHarmonization')
exchanges <- readWorksheet(ghgwkbk, 'Export')

lciafactors <- read.csv('~/Documents/GitHub/USEEIO/SI/USEEIO/LCIA_Factors.csv', stringsAsFactors = FALSE)
names(lciafactors)[1] <- 'Name'

flowharmonization$MasterName %in% lciafactors$Name
ghgfactors <- lciafactors %>%
  filter(Name %in% flowharmonization$MasterName) %>%
  select(Name, GCC)

intensities <- exchanges %>%
  select(FlowName, ProcessName, ProcessCode, FlowAmount, FlowUnit) %>%
  left_join(ghgfactors, by = c('FlowName' = 'Name'))

# Total kg of CO2 per $1 output.  
intensities_total <- intensities %>%
  group_by(ProcessName, ProcessCode) %>%
  summarize(GCC_total = sum(FlowAmount * GCC)) %>%
  arrange(ProcessCode)

# Load the crosswalk table so that we can find the average intensity by stage of supply chain.
fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_bea <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'BEA/formatted')

naicsCW <- read.csv(file.path(fp_crosswalks, 'naics_crosswalk_final.csv'), stringsAsFactors = FALSE)

naics_foodsystem <- naicsCW %>% 
  filter(food_system %in% c('partial', 'y')) %>%
  arrange(stage) %>%
  mutate(stage_code = case_when(
    stage %in% 'agriculture' ~ 'L1',
    stage %in% 'processing' ~ 'L2',
    stage %in% c('retail', 'wholesale') ~ 'L3',
    stage %in% 'foodservice' ~ 'L4a',
    stage %in% 'institutional' ~ 'L4b'
  ))


intensity_joined <- intensities_total %>%
  left_join(naics_foodsystem, by = c('ProcessCode' = 'BEA_389_code'))

# Get gross output of each stage so that we can calculate weighted average of intensity.
M <- read.csv(file.path(fp_bea, 'make2012.csv'), row.names = 1, check.names = FALSE)
gross_output <- data.frame(ProcessCode = row.names(M), output = M[,'T008'])

intensity_joined <- intensity_joined %>%
  left_join(gross_output) %>%
  mutate(output_FSC = output * proportion_food)

# We also need the household consumption volume - total of final demand for the ag products, processed food, and retail food
# Baseline final demand from useeiov1.1
all_final_demand <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIOv1.1/USEEIOv1.1_FinalDemand.csv', stringsAsFactors = FALSE)

demand_df <- all_final_demand %>%
  transmute(final_demand = X2007_US_Consumption / 1e6, ProcessCode = BEA_389_code)
                             
intensity_joined <- intensity_joined %>%
  left_join(demand_df) %>%
  mutate(demand_FSC = final_demand * proportion_food)

# Now get the average waste rate across all food groups by stage.
# Get baseline waste rate for each sector using the stage it belongs to and weighted average of the food categories in that sector
# Original version has equal proportions of all relevant food categories in each sector
# Later will be replaced with a correct weighted average
faopct <- read.csv(file.path(fp_crosswalks, 'fao_percentages_extended.csv'), stringsAsFactors = FALSE)
faopct <- faopct %>%
  mutate(L1 = loss_ag_production,
         L2 = 1 - (1 - loss_handling_storage) * (1 - loss_processing_packaging),
         L3 = loss_distribution,
         L4a = loss_consumption,
         L4b = loss_consumption)


waste_rate_bysector <- t(faopct[, naics_foodsystem$stage_code])
fao_category_weights <- naics_foodsystem %>% select(cereals:beverages)
baseline_waste_rate <- rowSums(waste_rate_bysector * fao_category_weights, na.rm = TRUE) / rowSums(fao_category_weights)

intensity_joined <- intensity_joined %>%
  left_join(data.frame(ProcessCode = naics_foodsystem$BEA_389_code, waste_rate = baseline_waste_rate))

# Get intensity and total volume, and waste rate by stage
intensity_summary <- intensity_joined %>%
  group_by(stage) %>%
  summarize(GHG_intensity = weighted.mean(GCC_total, output_FSC),
            volume = sum(output_FSC),
            waste_rate = weighted.mean(waste_rate, output_FSC))

intensity_summary_household <- intensity_joined %>%
  ungroup %>%
  filter(stage_code %in% c('L1','L2','L3')) %>%
  summarize(volume = sum(demand_FSC),
            waste_rate = weighted.mean(waste_rate, demand_FSC), 
            waste_total = waste_rate * volume) %>%
  mutate(stage = 'household')

# Create a basic figure with these parameters.
intensity_summary <- intensity_summary %>%
  mutate(GHG_total = GHG_intensity * volume, waste_total = waste_rate * volume)

bind_rows(intensity_summary, intensity_summary_household) %>%
  filter(!is.na(stage)) %>%
  write.csv('~/Dropbox/projects/foodwaste/FoodPolicy_MS/graphicalabstract/avgs.csv', row.names = FALSE)
