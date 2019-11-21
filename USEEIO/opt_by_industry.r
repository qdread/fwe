# Optimization analysis by individual industry and by individual food type
# QDR / FWE / 22 Oct 2019

# 1. Need to create function to output impact vector 
# Select any number of industries and any number of food types to implement the reduction, find demand vector and matrix, and calculate EEIO

# 2. Optimization function "wrapper" that takes a variable number of terms to optimize over, each of which represents a single industry-food type combination
# This can have additional constraints such that more than one are equal to one another.

# In general, try to avoid using functions that access variables in the global environment (bad practice) other than maybe the Python reticulate object.


# Function to calculate the demand reduction rates for all industries (intermediate and final) given a reduction across any number of industries and any number of food types
# mandatory arguments to supply are which industries to reduce intermediate and final demand, and which food types, and the proportion reduction
calc_all_reduction_rates <- function(W0_intermediate, W0_final, r, industries_reduce_intermediate, industries_reduce_final, foods_reduce_intermediate, foods_reduce_final, crosswalk = naics_foodsystem) {
  # For each industry, find the proportion of its output belonging to the foods to be reduced
  # Multiply the proportion reduction by the eligible food proportion to get the proportion reduction relative to full industry output
  table_intermediate <- crosswalk %>% 
    filter(BEA_389_code %in% industries_reduce_intermediate) %>%
    select(BEA_389_code, proportion_food, foods_reduce_intermediate)
  table_final <- crosswalk %>% 
    filter(BEA_389_code %in% industries_reduce_final) %>%
    select(BEA_389_code, proportion_food, foods_reduce_intermediate)
  
  proportion_intermediate <- table_intermediate$proportion_food * apply(table_intermediate[,foods_reduce_intermediate], 1, sum)
  proportion_final <- table_final$proportion_food * apply(table_final[,foods_reduce_final], 1, sum)
  
  # Now we have the eligible proportion for each industry, calculate the demand reduction rate needed to achieve given % waste reduction
  demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1
  
  demand_reduction_intermediate <- demand_change_fn(W0_intermediate, r, proportion_intermediate)
  demand_reduction_final <- demand_change_fn(W0_final, r, proportion_final)
  
  return(list(intermediate = demand_reduction_intermediate,
              final = demand_reduction_final))
  
}


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



# Function to get results from EEIO 
get_eeio_result <- function(c_factor, r_factor, c_names, r_names, i = 'no_name', crosswalk = naics_foodsystem) {
  
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

# Evaluation function used for optimization (takes in costs and returns environmental impacts given waste reduction scenario based on those costs)
eval_f_eeio <- function(x, category, W0, Wu, B, nu, p, W0_final, Wu_final, B_final, nu_final, p_final, p_food, Ctotal) {
  # Find reduction rates given costs x
  # We need a different W0, Wu, B, and possibly nu value for each element in the waste rate vector.
  r <- get_reduction_rates(x = x, W0 = W0, Wu = Wu, B = B, nu = nu, p = p, W0_final = W0_final, Wu_final = Wu_final, B_final = B_final, nu_final = nu_final, p_final = p_final)
  
  # Factors by which to multiply intermediate and final demand given the waste reductions calculated from costs x
  # Also multiply this by the proportion food for each of the sectors.
  intermediate_demand_change_factors <- demand_change_fn(W0 = W0, r = r$intermediate, p = p * p_food)
  final_demand_change_factors <- demand_change_fn(W0 = W0, r = r$final, p = p_final * p_food)
  
  # define the scenario values given x. 
  res <- get_eeio_result(c_factor = intermediate_demand_change_factors,
                         c_names = sector_long_names,
                         r_factor = final_demand_change_factors,
                         r_names = sector_short_names)
  # Return the impact value to be minimized.
  res$value[res$impact_category == category]
}