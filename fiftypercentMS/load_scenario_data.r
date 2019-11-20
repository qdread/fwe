# Script to separately load all the data to be used for different scenario analyses
# QDR / FWE / 07 May 2019

# Modified 29 May 2019: Use new routine for modifying intermediate and final demand values for scenarios.
# Modified 11 Jun 2019: Change crosswalk table and FAO percentage table to updated version.

# Step 0: Load packages, set file paths, read data ------------------------

library(tidyverse)
library(reticulate)

fp_fwe <- ifelse(dir.exists('~/Documents/GitHub'), '~/Documents/GitHub/fwe', '/research-home/qread/fwe')
fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_scenario <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_inputdata')
fp_bea <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'BEA/formatted')
fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
fp_useeio <- ifelse(dir.exists('~/Documents/GitHub'), '~/Documents/GitHub/USEEIO', '/research-home/qread/USEEIO')

faopct <- read.csv(file.path(fp_crosswalks, 'fao_percentages_extended.csv'), stringsAsFactors = FALSE)
naicsCW <- read.csv(file.path(fp_crosswalks, 'naics_crosswalk_final.csv'), stringsAsFactors = FALSE)

# Step 1: Get baseline loss rate for each row in BEA table ----------------

# Weighting NAICS codes by FAO loss rates to get baseline loss rate
# Convert this to get new loss rate for each scenario

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

# Get baseline waste rate for each sector using the stage it belongs to and weighted average of the food categories in that sector
# Original version has equal proportions of all relevant food categories in each sector
# Later will be replaced with a correct weighted average
faopct <- faopct %>%
  mutate(L1 = loss_ag_production,
         L2 = 1 - (1 - loss_handling_storage) * (1 - loss_processing_packaging),
         L3 = loss_distribution,
         L4a = loss_consumption,
         L4b = loss_consumption)

waste_rate_bysector <- t(faopct[, naics_foodsystem$stage_code])
fao_category_weights <- naics_foodsystem %>% select(cereals:beverages)
baseline_waste_rate <- rowSums(waste_rate_bysector * fao_category_weights, na.rm = TRUE) / rowSums(fao_category_weights)

# Load make and use tables
M <- read.csv(file.path(fp_bea, 'make2012.csv'), row.names = 1, check.names = FALSE)
U <- read.csv(file.path(fp_bea, 'use2012.csv'), row.names = 1, check.names = FALSE)

# Load demand codes
all_codes <- read.csv(file.path(fp_crosswalks, 'all_codes.csv'), stringsAsFactors = FALSE)

# Step 2: Source model code -----------------------------------------------

# Source Python script which runs model
# If run on server, specify we're using python3
if (dir.exists('/nfs/fwe-data')) use_python('/usr/bin/python3')
source_python(file.path(fp_fwe, 'USEEIO/eeio_lcia.py'))

# Source R script which builds models
source(file.path(fp_useeio, 'R/Model Build Scripts/USEEIO2012_buildfunction.R'))
model_build_path <- file.path(fp_useeio, 'useeiopy/Model Builds')

# Master function to run steps 3 through 6
# Takes several inputs: 
# c_factor is the factor to multiply the intermediate column inputs (vector), 
# r_factor is the factor to multiply the rows of the final demand column (vector),   
# c_names is the names of the columns to multiply by c_factor (vector with same length as c_factor)
# r_names is the names of the rows to multiply by r_factor (vector with same length as r_factor)
# i is the name of the scenario
# By default the only final demand column we modify is F01000: personal consumption expenditures (PCE)

get_eeio_result <- function(c_factor, r_factor, c_names, r_names, i = 'no_name') {

  # Step 3. Build USEEIO with specifications for modifying intermediate and final demand.
  build_USEEIO(outputfolder = file.path(model_build_path, paste0('scenario_', i)),
               model = paste0('scenario_', i),
               usetablefile = file.path(fp_bea, 'use2012.csv'),
               maketablefile = file.path(fp_bea, 'make2012.csv'),
               code_path = fp_useeio,
               intermediate_columns_modify = c_names,
               intermediate_change_factor = c_factor,
               final_rows_modify = r_names,
               final_columns_modify = c('F01000'),
               final_change_factor = r_factor
  )
  # Step 4. Extract demand vector for food system from scenario.
  # Join this with the food system proportions and with the correct demand codes (full description names)
  all_final_demand <- read.csv(file.path(model_build_path, paste0('scenario_', i), paste0('scenario_', i, '_FinalDemand.csv')), stringsAsFactors = FALSE) %>%
    left_join(naics_foodsystem, by = c('BEA_389_code', 'BEA_389_def')) %>% 
    filter(!is.na(proportion_food)) %>%
    left_join(all_codes, by = c('BEA_389_code' = 'sector_code_uppercase'))
  # Convert demand vector to separate list of codes and values
  final_demand_list <- with(all_final_demand, list(codes = as.list(sector_desc_drc), values = as.list(X2012_US_Consumption * proportion_food)))
  # Step 5. Run the model!
  eeio_result <- eeio_lcia(paste0('scenario_', i), final_demand_list$values, final_demand_list$codes) 
  eeio_result <- data.frame(scenario = i, impact_category = rownames(eeio_result), value = eeio_result$Total, stringsAsFactors = FALSE)
  # Step 6. Delete the intermediate files (entire model build folder) 
  unlink(file.path(model_build_path, paste0('scenario_', i)), recursive = TRUE)
  
  return(eeio_result)
}

