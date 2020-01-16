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
fp_bea <- file.path(ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data'), 'BEA/formatted')
fp_temp <- ifelse(dir.exists('~/Documents/temp'), '~/Documents/temp', '/tmp')
fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
fp_useeio <- ifelse(dir.exists('~/Dropbox'), '~/Dropbox/projects/foodwaste/Code/USEEIO-master', '/research-home/qread/USEEIO')

source(file.path(fp_github, 'USEEIO/modify_make_and_use.r'))

faopct <- readWorksheetFromFile(file.path(fp_crosswalks, 'fao_percentages.xlsx'), sheet = 1)
naicsCW <- read.csv(file.path(fp_crosswalks, 'naics_lafa_qfahpd_crosswalk_modified.csv'), stringsAsFactors = FALSE)

# Step 1: Get baseline loss rate for each row in BEA table ----------------

# Weighting NAICS codes by FAO loss rates to get baseline loss rate
# Convert this to get new loss rate for each scenario

naics_foodsystem <- naicsCW %>% 
  filter(food_system %in% c('partial', 'y'), nchar(FAO_category) > 0) %>%
  arrange(stage) %>%
  mutate(proportion_food = if_else(is.na(proportion_food), 1, proportion_food)) %>%
  mutate(stage_code = case_when(
    stage %in% 'agriculture' ~ 'L1',
    stage %in% 'processing' ~ 'L2',
    TRUE ~ 'L3'
  ))

# Parse the FAO category comma separated lists of numbers into vectors.
FAO_category_vectors <- sapply(naics_foodsystem$FAO_category, function(x) {
  if (nchar(x) == 0) return(NA)
  eval(parse(text = paste0('c(',x,')')))
})

# Get baseline waste rate for each one
baseline_waste_rate <- map2_dbl(FAO_category_vectors, 1:nrow(naics_foodsystem), ~ mean(faopct[.x, naics_foodsystem$stage_code[.y]])) %>% as.numeric

# Source Python script which runs model
# If run on server, specify we're using python3
if (dir.exists('/nfs/qread-data')) use_python('/usr/bin/python3')
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
# v is the demand vector and i is the name of the scenario

get_eeio_result <- function(v, i = 'no_name') {
  # Step 3. Create modified make and use tables for each scenario and write them to CSVs.
  MU_modified <- modify_and_renormalize_make_and_use(M, U, v, naics_foodsystem$BEA_389_code)
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

