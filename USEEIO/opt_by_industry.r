# Optimization analysis by individual industry and by individual food type
# QDR / FWE / 22 Oct 2019

# 1. Need to create function to output impact vector 
# Select any number of industries and any number of food types to implement the reduction, find demand vector and matrix, and calculate EEIO

# 2. Optimization function "wrapper" that takes a variable number of terms to optimize over, each of which represents a single industry-food type combination
# This can have additional constraints such that more than one are equal to one another.

# In general, try to avoid using functions that access variables in the global environment (bad practice) other than maybe the Python reticulate object.


# Function to calculate the demand reduction rates for all sectors (intermediate and final) given a reduction across any number of sectors and any number of food types
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
