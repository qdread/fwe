# Script to get the DIRECT impacts of each sector from EEIO
# Baseline and one with waste 100% reduced at each of the stages.

# Modified 17 Sept 2019: Also do this for each of the 50% cases that are the "best" so we can see how much each food contributed to the reduction.
# Modified 18 July 2019: include a way to return the final demand from each of the runs, because we want to get contributions per dollar of demand.

# Load data ---------------------------------------------------------------

# Load scenario data
source('~/fwe/USEEIO/load_scenario_data.r')
source_python(file.path(fp_fwe, 'USEEIO/eeio_lcia_contributions.py'))

### Sector codes and full names
sector_stage_codes <- naics_foodsystem$stage_code
sector_long_names <- all_codes$sector_desc_drc[match(naics_foodsystem$BEA_389_code, all_codes$sector_code_uppercase)]
sector_short_names <- naics_foodsystem$BEA_389_code
final_demand_sector_codes <- sector_stage_codes
final_demand_sector_codes[final_demand_sector_codes %in% c('L1', 'L2', 'L3')] <- 'L5'

# Function definitions ----------------------------------------------------

# Function to get change in demand given original waste rate W0, reduction rate r, and proportion of the sector's output that is food p
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# Exponential or Richards curves for new waste rate given cost x, old waste rate W0, unavoidable waste rate Wu, and parameters B and nu
waste_rate_by_cost <- function(x, W0, Wu, B, nu) {
  # If nu = 1, this becomes the exponential curve
  A <- (W0 - (2 ^ (-1/nu)) * Wu) / (1 - 2 ^ (-1/nu))
  pmin(W0, A + (Wu - A)/((1 + exp(-B*x))^(1/nu)))
}

# function to calculate the demand reduction rates for all sectors (intermediate and final), given the costs for the 6 stages, and the parameters described above
get_reduction_rates <- function(x, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, W0_sectors_final, Wu_sectors_final, B_sectors_final, nu_sectors_final, p_sectors_final, sector_stage_codes, final_demand_sector_codes) {
  names(x) <- c('L1', 'L2', 'L3', 'L4a', 'L4b', 'L5')
  # Intermediate reduction rates
  allocated_costs_intermediate <- x[sector_stage_codes] * p_sectors
  Wnew_sectors <- waste_rate_by_cost(x = allocated_costs_intermediate, W0 = W0_sectors, Wu = Wu_sectors, B = B_sectors, nu = nu_sectors)
  r_sectors_intermediate <- 1 - Wnew_sectors / W0_sectors 
  
  # Final reduction rates
  allocated_costs_final <- x[final_demand_sector_codes] * p_sectors_final
  Wnew_sectors_final <- waste_rate_by_cost(x = allocated_costs_final, W0 = W0_sectors_final, Wu = Wu_sectors_final, B = B_sectors_final, nu = nu_sectors_final) 
  r_sectors_final <- 1 - Wnew_sectors_final / W0_sectors
  
  return(list(intermediate = r_sectors_intermediate,
              final = r_sectors_final))
}

# Modified function to get full contributions
get_eeio_contributions <- function(c_factor, r_factor, c_names, r_names, i = 'no_name') {
  
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
  eeio_result <- eeio_lcia_contributions(paste0('scenario_', i), final_demand_list$values, final_demand_list$codes) 
  eeio_result_df <- data.frame(scenario = i, impact_category = rownames(eeio_result), eeio_result, stringsAsFactors = FALSE, check.names = FALSE)
  # Step 6. Delete the intermediate files (entire model build folder) 
  unlink(file.path(model_build_path, paste0('scenario_', i)), recursive = TRUE)
  
  return(list(lcia_contr = eeio_result_df, demand = final_demand_list))
}


# Get direct impacts from baseline and the 6 scenarios ----------------------------------------------

reduction_all1 <- rbind(rep(0,6), diag(6), rep(1,6)) %>%
  as.data.frame() %>%
  setNames(c('L1','L2','L3','L4a','L4b','L5'))

# Create list from grid
reduction_rate_grid_list <- setNames(split(reduction_all1, seq(nrow(reduction_all1))), rownames(reduction_all1))

# For each combination scenario, put together a vector of demand reduction factors for intermediate demand with values and names equal to the column names of the DRC matrix
# Also put together a vector of demand reduction factors for final demand with values and names equal to the row names of the DRC matrix. This will modify the PCE column of final demand.

get_reduction <- function(reduction_by_stage, scenario_id) {
  intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[sector_stage_codes], naics_foodsystem$proportion_food))
  final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[final_demand_sector_codes], naics_foodsystem$proportion_food))
  get_eeio_contributions(c_factor = intermediate_demand_change_factors,
                         c_names = sector_long_names,
                         r_factor = final_demand_change_factors,
                         r_names = sector_short_names,
                         i = scenario_id)
}

eeio_result_allcontributions <- mapply(get_reduction, reduction_by_stage = reduction_rate_grid_list, scenario_id = 1:length(reduction_rate_grid_list), SIMPLIFY = FALSE)

eeio_result_allcontributions_df <- map_dfr(eeio_result_allcontributions, 'lcia_contr')
eeio_result_allcontributions_df <- cbind(stage_reduced = rep(c('baseline','L1','L2','L3','L4a','L4b','L5','zerowaste'), each = 21), eeio_result_allcontributions_df)

write.csv(eeio_result_allcontributions_df, file.path(fp_output, 'lcia_contributions.csv'), row.names = FALSE)

eeio_demand_list <- map2_dfr(eeio_result_allcontributions, c('baseline','L1','L2','L3','L4a','L4b','L5','zerowaste'),
                             ~ data.frame(stage_reduced = .y, 
                                          BEA_389_code = do.call(c, .x$demand$codes),
                                          final_demand = do.call(c, .x$demand$values)))

write.csv(eeio_demand_list, file.path(fp_output, 'lcia_contributions_finaldemand.csv'), row.names = FALSE)


# Get direct impacts from the best 50% scenarios --------------------------

# Load the results showing which are best.
# should recreate trueseq_withcis from sixstage_sens_figs.r (or save that and load it.)

trueseq <- read.csv(file.path(fp_output, 'bestsequences.csv'), stringsAsFactors = FALSE)
reduction_best <- trueseq %>% filter(rowSums(.[,1:6]) > 0)

# Only need to do the best trajectories once each
reduction_best <- reduction_best[match(unique(reduction_best$scenario), reduction_best$scenario), ]

reduction_best_rates <- reduction_best[,1:6]

# Save scenario parameters so we can join them again
write.csv(rbind(reduction_best[,1:7], cbind(reduction_all1, scenario = c('baseline','L1','L2','L3','L4a','L4b','L5','zerowaste'))), file.path(fp_output, 'scenario_parameters_best50.csv'), row.names = FALSE)

# Create list from grid
reduction_rate_grid_list <- setNames(split(reduction_best_rates, seq(nrow(reduction_best_rates))), rownames(reduction_best_rates))

eeio_result_allcontributions <- mapply(get_reduction, reduction_by_stage = reduction_rate_grid_list, scenario_id = 1:length(reduction_rate_grid_list), SIMPLIFY = FALSE)

eeio_result_allcontributions_df <- map_dfr(eeio_result_allcontributions, 'lcia_contr')
eeio_result_allcontributions_df$scenario <-  rep(reduction_best$scenario, each = 21)

write.csv(eeio_result_allcontributions_df, file.path(fp_output, 'lcia_contributions_best50.csv'), row.names = FALSE)

eeio_demand_list <- map2_dfr(eeio_result_allcontributions, reduction_best$scenario,
                             ~ data.frame(scenario = .y, 
                                          BEA_389_code = do.call(c, .x$demand$codes),
                                          final_demand = do.call(c, .x$demand$values)))

write.csv(eeio_demand_list, file.path(fp_output, 'lcia_contributions_best50_finaldemand.csv'), row.names = FALSE)
