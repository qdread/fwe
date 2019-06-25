# Sensitivity analysis of 50% and 100% food waste reduction.
# To be run remotely in parallel.

# This version created 25 June 2019.

# Load data ---------------------------------------------------------------

library(parallel)
n_cores <- 8

# Load scenario data
source('~/fwe/USEEIO/load_scenario_data.r')

### Sector codes and full names
sector_stage_codes <- naics_foodsystem$stage_code
sector_long_names <- all_codes$sector_desc_drc[match(naics_foodsystem$BEA_389_code, all_codes$sector_code_uppercase)]
sector_short_names <- naics_foodsystem$BEA_389_code
final_demand_sector_codes <- sector_stage_codes
final_demand_sector_codes[final_demand_sector_codes %in% c('L1', 'L2', 'L3')] <- 'L5'

# Function definitions ----------------------------------------------------

# Function to get change in demand given original waste rate W0, reduction rate r, and proportion of the sector's output that is food p
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# Function to generate a sampling draw for proportion food, if it's less than 1.
# Accepts input of p (vector) and factor for uncertainty of beta distribution. So each is beta(fp, f(1-p))
proportion_food_draw <- function(p, f = 100) rbeta(n = length(p), shape1 = f * p, shape2 = f * (1 - p))

# Function to generate a sampling draw for the baseline waste rates.
# Assume no relationship among the various loss rates, whether within category or within stage
# This could be modified by replacing beta distributions with Dirichlet distributions
# Accepts input of w, which is a matrix of the loss rates, and f, factor for uncertainty of beta distribution
baseline_rate_table_draw <- function(w, f = 100) {
  res <- apply(w, 2, function(p) rbeta(n = length(p), shape1 = f * p, shape2 = f * (1 - p)))
  res[!is.finite(res)] <- NA
  res
}

# Function to get results from EEIO with different values from the sensitivity analysis.
get_eeio_result_sensitivity <- function(c_factor, r_factor, c_names, r_names, i = 'no_name', crosswalk = naics_foodsystem) {
  
  # Build USEEIO with specifications for modifying intermediate and final demand.
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
  # Extract demand vector for food system from scenario.
  # Join this with the food system proportions and with the correct demand codes (full description names)
  all_final_demand <- read.csv(file.path(model_build_path, paste0('scenario_', i), paste0('scenario_', i, '_FinalDemand.csv')), stringsAsFactors = FALSE) %>%
    left_join(crosswalk, by = c('BEA_389_code', 'BEA_389_def')) %>% 
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



# Generate values for 50% scenarios ---------------------------------------

n_draws <- 100 # Number of rounds of sensitivity analysis
uncertainty_factor <- 100 # The higher this number, the LOWER the uncertainty.
set.seed(111)

# Only use 0% reduction (baseline) and 50% reduction for each stage

rate_levels <- c(0, 0.5, 1)
reduction_rate_grid <- expand.grid(L1 = rate_levels, L2 = rate_levels, L3 = rate_levels, L4a = rate_levels, L4b = rate_levels, L5 = rate_levels)

# Create list from grid
reduction_rate_grid_list <- split(reduction_rate_grid, seq(nrow(reduction_rate_grid))) %>%
  map(unlist)

# Take random draws around mean for FAO waste rate
faopct_list <- replicate(n_draws, baseline_rate_table_draw(w = faopct %>% select(loss_ag_production:loss_consumption), f = uncertainty_factor), simplify = FALSE)

# Get L1 - L4b values from each draw
faopct_list <- map(faopct_list, ~ .x %>%
                     as.data.frame %>%
                     mutate(L1 = loss_ag_production,
                            L2 = 1 - (1 - loss_handling_storage) * (1 - loss_processing_packaging),
                            L3 = loss_distribution,
                            L4a = loss_consumption,
                            L4b = loss_consumption))

# Take random draws around mean for proportion food in each sector
proportion_food_list <- replicate(n_draws, proportion_food_draw(p = naics_foodsystem$proportion_food, f = uncertainty_factor), simplify = FALSE)

waste_rate_bysector_list <- map(faopct_list, ~ t(.x[, naics_foodsystem$stage_code]))
fao_category_weights <- naics_foodsystem %>% select(cereals:beverages) # Do not bother to do sensitivity analysis for this.
baseline_waste_rate_list <- map(waste_rate_bysector_list, ~ rowSums(.x * fao_category_weights, na.rm = TRUE) / rowSums(fao_category_weights))


# Run scenarios in parallel -----------------------------------------------

get_reduction_from_list <- function(reduction_rate, baseline_waste_rate, proportion_food, scenario_id) {
  intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_rate[sector_stage_codes], proportion_food))
  final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_rate[final_demand_sector_codes], proportion_food))
  get_eeio_result_sensitivity(c_factor = intermediate_demand_change_factors,
                              c_names = sector_long_names,
                              r_factor = final_demand_change_factors,
                              r_names = sector_short_names,
                              i = scenario_id,
                              crosswalk = naics_foodsystem %>% mutate(proportion_food = !!proportion_food))
}

# Cross the lists of reduction rates with the different parameters
# Total number of models to run: number of draws * number of reduction rate combinations
baseline_waste_rate_combos <- cross2(reduction_rate_grid_list, baseline_waste_rate_list)
proportion_food_combos <- cross2(reduction_rate_grid_list, proportion_food_list)
																						 
sensitivity_arguments <- list(reduction_rate = map(baseline_waste_rate_combos, 1),
                              baseline_waste_rate = map(baseline_waste_rate_combos, 2),
                              proportion_food = map(proportion_food_combos, 2),
                              scenario_id = as.list(1:length(baseline_waste_rate_combos)))

eeio_result_grid_sensitivity <- with(sensitivity_arguments, mcmapply(get_reduction_from_list, reduction_rate, baseline_waste_rate, proportion_food, scenario_id, mc.cores = n_cores, SIMPLIFY = FALSE))


# Match output with arguments to get CIs ----------------------------------

grid_sensitivity_df <- map2_dfr(sensitivity_arguments, eeio_result_grid_sensitivity, ~ data.frame(.x$reduction_rate, .y))

grid_sensitivity_CIs <- grid_sensitivity_df %>%
  group_by(L1, L2, L3, L4a, L4b, L5, impact_category) %>%
  do(quantile(.$value, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>% t %>% as.data.frame %>% setNames(c('q025', 'q25', 'q50', 'q75', 'q975')))

write.csv(grid_sensitivity_CIs, file.path(fp_output, 'sensitivity_grid_CIs.csv'), row.names = FALSE)


# Save output separately --------------------------------------------------

# Assign an ID to each of the parameter combinations so that they can be coupled with the results

n_rates <- length(reduction_rate_grid_list)
n_categories <- nrow(eeio_result_grid_sensitivity[[1]])

grid_sensitivity_df$draw_id <- rep(1:n_draws, each = n_rates * n_categories)
write.csv(grid_sensitivity_df, file.path(fp_output, 'sensitivity_grid_alldraws.csv'), row.names = FALSE)
