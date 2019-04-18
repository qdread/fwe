# Fully updated scenario script 
# using baseline loss rates from FAO and using the corrected way of modifying and renormalizing the make and use tables
# QDR / FWE / 18 April 2019



# Step 0: Load packages, set file paths, read data ------------------------

library(tidyverse)
library(XLConnect)
library(reticulate)

fp_github <- ifelse(dir.exists('~/Documents/GitHub'), '~/Documents/GitHub/fwe', '~/fwe')
fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_scenario <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_inputdata')
fp_bea <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'BEA/formatted')
fp_temp <- ifelse(dir.exists('~/Documents/temp'), '~/Documents/temp', '/nfs/qread-data/temp')
fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
fp_useeio <- ifelse(dir.exists('~/Dropbox'), '~/Dropbox/projects/foodwaste/Code/USEEIO-master', '~/USEEIO')

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


# Step 2: Create demand modification factors by scenario ------------------

# Simulate 7 different scenarios in addition to the baseline.
# 25% reduction of waste rate in L1
# 25% reduction of waste rate in L2
# 25% reduction of waste rate in L3
# ~13.4% reduction of waste rate in L1&L2 (this works out to "total" 25% reduction in waste rate)
# ~13.4% reduction in L1&L3
# ~13.4% reduction in L2&L3
# ~9.1% waste reduction in all 3 stages (this works out to "total" 25% reduction in waste rate)

# Function to calculate factor of demand change you get by reducing waste by a certain amount
# w_orig is original waste
# r is percent reduction in waste
# p is proportion of the sector representing the food system
demand_change_fn <- function(w_orig, r, p) p * ((1 - w_orig) / (1 - (1 - r) * w_orig) - 1) + 1

# Scenarios are waste reduction across 0-3 sectors
# We calculate the new demand as a fraction of the original demand, accounting for the sectors that are only partially food
reduction_rates <- e1071::bincombinations(3) 
reduction_rates <- 1 - (0.75 ^ sweep(reduction_rates, 1, apply(reduction_rates, 1, sum), '/'))
colnames(reduction_rates) <- c('L1', 'L2', 'L3')
reduction_rates[is.na(reduction_rates)] <- 0

demand_change_factors <- apply(reduction_rates, 1, function(v) {
  reduction_rate_bysector <- v[naics_foodsystem$stage_code]
  demand_change_fn(baseline_waste_rate, reduction_rate_bysector, naics_foodsystem$proportion_food)
})

# Give the scenarios names based on which levels are reduced.
scenario_names <- c('baseline', apply(reduction_rates, 1, function(v) paste(names(v)[v>0], collapse = ''))[-1])
colnames(demand_change_factors) <- paste0('demandchange_', scenario_names)

loss_rates <- cbind(naics_foodsystem, demand_change_factors)

# Step 3: Create modified make and use tables for each scenario -----------

# Load make and use tables
M <- read.csv(file.path(fp_bea, 'make2012.csv'), row.names = 1, check.names = FALSE)
U <- read.csv(file.path(fp_bea, 'use2012.csv'), row.names = 1, check.names = FALSE)

MU_modified <- map(loss_rates %>% select(starts_with('demandchange')), ~ modify_and_renormalize_make_and_use(M, U, .x, loss_rates$BEA_389_code))

# Write the make and use tables to CSVs
iwalk(MU_modified, ~ write.csv(.x$M, file = file.path(fp_temp, paste0('make_', .y, '.csv'))))
iwalk(MU_modified, ~ write.csv(.x$U, file = file.path(fp_temp, paste0('use_', .y, '.csv'))))


# Step 4: Build USEEIO with each of the make and use tables ---------------

# (Write the resulting model build files to a directory.)

source(file.path(fp_useeio, 'R/Model Build Scripts/USEEIO2012_buildfunction.R'))
model_build_path <- file.path(fp_useeio, 'useeiopy/Model Builds')

# Walk through the list of scenario names and build the model with the appropriate make and use tables.
# Note: it takes a few seconds to build each model.
walk(scenario_names, ~ build_USEEIO(outputfolder = file.path(model_build_path, paste0('scenario_', .x)),
                                    model = paste0('scenario_', .x),
                                    usetablefile = file.path(fp_temp, paste0('use_demandchange_', .x, '.csv')),
                                    maketablefile = file.path(fp_temp, paste0('make_demandchange_', .x, '.csv')),
                                    code_path = fp_useeio
                                    )
     )


# Step 5: Extract demand vectors for food system from each scenario -------

# Read final demand CSV from each built model.
all_final_demand <- map(scenario_names, ~ read.csv(file.path(model_build_path, paste0('scenario_', .x), paste0('scenario_', .x, '_FinalDemand.csv')), stringsAsFactors = FALSE))
all_codes <- read.csv(file.path(fp_crosswalks, 'all_codes.csv'), stringsAsFactors = FALSE)

# Join each full final demand vector with food system proportions 
# Also match these with the correct codes that have full description names
all_final_demand <- map(all_final_demand, ~ .x %>%
                          left_join(naics_foodsystem) %>% 
                          filter(!is.na(proportion_food)) %>%
                          left_join(all_codes, by = c('BEA_389_code' = 'sector_code_uppercase')))

# Convert each demand vector to separate lists of codes and values
final_demand_lists <- map(all_final_demand, ~ list(codes = as.list(.x$sector_desc_drc), values = as.list(.x$X2012_US_Consumption * .x$proportion_food)))


# Step 6. Run the models! -------------------------------------------------

# Source Python script which runs model
source_python(file.path(fp_github, 'USEEIO/eeio_lcia.py'))

# Run all scenarios. (takes a couple seconds per scenario)
eeio_result <- map2(final_demand_lists, scenario_names, ~ eeio_lcia(paste0('scenario_', .y), .x$values, .x$codes))
impact_categories <- rownames(eeio_result[[1]])
eeio_result <- map2_dfr(eeio_result, scenario_names, ~ data.frame(scenario = .y, .x))
eeio_result <- with(eeio_result, data.frame(scenario, impact_category = impact_categories, value = Total, stringsAsFactors = FALSE))


# Step 7. Write result to file and delete model files ---------------------

write.csv(eeio_result, file.path(fp_output, 'fao_scenario_lcia_results.csv'), row.names = FALSE)

# Delete the intermediate files (edited use tables, edited make tables, and model build folder) 
invisible(file.remove(c(file.path(fp_temp, paste0('use_demandchange_', scenario_names, '.csv')),
                        file.path(fp_temp, paste0('make_demandchange_', scenario_names, '.csv')))))
unlink(file.path(model_build_path, paste0('scenario_', scenario_names)), recursive = TRUE)
