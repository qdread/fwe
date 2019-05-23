# Script to separately load all the data to be used for different scenario analyses
# QDR / FWE / 07 May 2019



# Step 0: Load packages, set file paths, read data ------------------------

library(tidyverse)
library(XLConnect)
library(reticulate)
library(furrr)

fp_github <- ifelse(dir.exists('~/Documents/GitHub'), '~/Documents/GitHub/fwe', '/research-home/qread/fwe')
fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_scenario <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_inputdata')
fp_bea <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'BEA/formatted')
fp_temp <- ifelse(dir.exists('~/Documents/temp'), '~/Documents/temp', '/tmp')
fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
fp_useeio <- ifelse(dir.exists('~/Dropbox'), '~/Dropbox/projects/foodwaste/Code/USEEIO-master', '/research-home/qread/USEEIO')

source(file.path(fp_github, 'USEEIO/modify_retotal_make_and_use.r'))

faopct <- readWorksheetFromFile(file.path(fp_crosswalks, 'fao_percentages.xlsx'), sheet = 1)
naicsCW <- read.csv(file.path(fp_crosswalks, 'naics_crosswalk_allproportions.csv'), stringsAsFactors = FALSE)

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
fao_category_weights <- naics_foodsystem %>% select(cereals:eggs)
baseline_waste_rate <- rowSums(waste_rate_bysector * fao_category_weights) / rowSums(fao_category_weights)

# Source Python script which runs model
# If run on server, specify we're using python3
if (dir.exists('/nfs/fwe-data')) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'USEEIO/eeio_lcia.py'))

# Source R script which builds models
source(file.path(fp_useeio, 'R/Model Build Scripts/USEEIO2012_buildfunction.R'))
model_build_path <- file.path(fp_useeio, 'useeiopy/Model Builds')

# Load make and use tables
M <- read.csv(file.path(fp_bea, 'make2012.csv'), row.names = 1, check.names = FALSE)
U <- read.csv(file.path(fp_bea, 'use2012.csv'), row.names = 1, check.names = FALSE)

# Load demand codes
all_codes <- read.csv(file.path(fp_crosswalks, 'all_codes.csv'), stringsAsFactors = FALSE)

if (!dir.exists(file.path(fp_temp, 'fwe'))) dir.create(file.path(fp_temp, 'fwe'))
fp_mu <- file.path(fp_temp, 'fwe')

# Master function to run steps 3 through 7
# Takes several inputs: v is the demand vector, c_mod are the columns to modify (same length as v), r_mod are the rows to modify, and i is the name of the scenario

get_eeio_result <- function(v, i = 'no_name') {
  # Step 3. Create modified make and use tables for each scenario and write them to CSVs.
  MU_modified_intermediate <- modify_make_and_use(M, U, R = v, c_int_mod = c_mod, c_final_mod = 'F01000', r_mod = r_mod)
  MU_modified <- retotal_make_and_use(M = MU_modified_intermediate$M, U = MU_modified_intermediate$U)
  write.csv(MU_modified$M, file = file.path(fp_mu, paste0('make_', i, '.csv')))
  write.csv(MU_modified$U, file = file.path(fp_mu, paste0('use_', i, '.csv')))
  # Step 4. Build USEEIO with those make and use tables
  build_USEEIO(outputfolder = file.path(model_build_path, paste0('scenario_', i)),
               model = paste0('scenario_', i),
               usetablefile = file.path(fp_mu, paste0('use_', i, '.csv')),
               maketablefile = file.path(fp_mu, paste0('make_', i, '.csv')),
               code_path = fp_useeio
  )
  # Step 5. Extract demand vector for food system from scenario.
  # Join this with the food system proportions and with the correct demand codes (full description names)
  all_final_demand <- read.csv(file.path(model_build_path, paste0('scenario_', i), paste0('scenario_', i, '_FinalDemand.csv')), stringsAsFactors = FALSE) %>%
    left_join(naics_foodsystem, by = c('BEA_389_code', 'BEA_389_def')) %>% 
    filter(!is.na(proportion_food)) %>%
    left_join(all_codes, by = c('BEA_389_code' = 'sector_code_uppercase'))
  # Convert demand vector to separate list of codes and values
  final_demand_list <- with(all_final_demand, list(codes = as.list(sector_desc_drc), values = as.list(X2012_US_Consumption * proportion_food)))
  # Step 6. Run the model!
  eeio_result <- eeio_lcia(paste0('scenario_', i), final_demand_list$values, final_demand_list$codes) 
  eeio_result <- data.frame(scenario = i, impact_category = rownames(eeio_result), value = eeio_result$Total, stringsAsFactors = FALSE)
  # Step 7. Delete the intermediate files (edited use tables, edited make tables, and model build folder) 
  invisible(file.remove(c(file.path(fp_mu, paste0('use_', i, '.csv')),
                          file.path(fp_mu, paste0('make_', i, '.csv')))))
  unlink(file.path(model_build_path, paste0('scenario_', i)), recursive = TRUE)
  
  return(eeio_result)
}

