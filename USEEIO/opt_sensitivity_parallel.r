# Sensitivity analysis of optimization
# QDR / FWE / 26 June 2019

# Load data ---------------------------------------------------------------

library(parallel)
library(Rsolnp)
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



# Evaluation function used for optimization (takes in costs and returns environmental impacts given waste reduction scenario based on those costs)
eval_f_eeio <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, W0_sectors_final, Wu_sectors_final, B_sectors_final, nu_sectors_final, p_sectors_final, sector_stage_codes, final_demand_sector_codes, Ctotal) {
  # Find reduction rates given costs x
  # We need a different W0, Wu, B, and possibly nu value for each element in the waste rate vector.
  r_sectors <- get_reduction_rates(x = x, W0_sectors = W0_sectors, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = p_sectors, W0_sectors_final = W0_sectors_final, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = p_sectors_final, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes)
  
  # Factors by which to multiply intermediate and final demand given the waste reductions calculated from costs x
  intermediate_demand_change_factors <- demand_change_fn(W0 = W0_sectors, r = r_sectors$intermediate, p = p_sectors)
  final_demand_change_factors <- demand_change_fn(W0 = W0_sectors, r = r_sectors$final, p = p_sectors_final)
  
  # define the scenario values given x. 
  res <- get_eeio_result(c_factor = intermediate_demand_change_factors,
                         c_names = sector_long_names,
                         r_factor = final_demand_change_factors,
                         r_names = sector_short_names)
  # Return the impact value to be minimized.
  res$value[res$impact_category == category]
}

# Another version of constraint function that just returns the sum of the x values. For Rsolnp package.
eval_eq_total <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, W0_sectors_final, Wu_sectors_final, B_sectors_final, nu_sectors_final, p_sectors_final, sector_stage_codes, final_demand_sector_codes, Ctotal) {
  return(sum(x))
}


# Random draws of parameters ----------------------------------------------

n_draws <- 100
set.seed(666)

# Get parameters for abatement curves from Refed data.
refed_params <- read.csv(file.path(fp_crosswalks, 'refed_testvalues.csv'), stringsAsFactors = FALSE) %>%
  mutate(Wu = 1 - addressable/net,
         W1 = 1 - diversion.potential/net,
         C1 = cost,
         stage_code = c('L1', 'L2', 'L3', 'L4a', 'L4b', 'L5'))

# Get parameters for each sector using stage level values
# Wu: Unavoidable waste rate 
Wu_sectors <- baseline_waste_rate * refed_params$Wu[match(sector_stage_codes, refed_params$stage_code)]
Wu_sectors_final <- baseline_waste_rate * refed_params$Wu[match(final_demand_sector_codes, refed_params$stage_code)]

# W1: Diverted waste amount at C1
W1_sectors <- baseline_waste_rate * refed_params$W1[match(sector_stage_codes, refed_params$stage_code)]
W1_sectors_final <- baseline_waste_rate * refed_params$W1[match(sector_stage_codes, refed_params$stage_code)]

# Find the proportion of waste reduction dollars allocated to each sector within each stage of the food supply chain
# Use the baseline values for gross output from each sector to get the weights. (this is T008 in the make table)
gross_outputs <- M[sector_short_names, 'T008']
gross_outputs_by_stage <- tapply(gross_outputs, sector_stage_codes, sum)
proportion_gross_outputs <- gross_outputs / gross_outputs_by_stage[sector_stage_codes]

# Also do this proportion for the final ones.
gross_outputs_by_stage_final <- tapply(gross_outputs, final_demand_sector_codes, sum)
proportion_gross_outputs_final <- gross_outputs / gross_outputs_by_stage_final[final_demand_sector_codes]

# C1: Cost to achieve W1
# The C1 for each stage needs to be multiplied by the sector's proportion output of the stage it belongs to, to get the C1 for each sector.
C1_sectors <- refed_params$C1[match(sector_stage_codes, refed_params$stage_code)] * proportion_gross_outputs
C1_sectors_final <- refed_params$C1[match(final_demand_sector_codes, refed_params$stage_code)] * proportion_gross_outputs_final

