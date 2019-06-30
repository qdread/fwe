# Draft code for new scenario analysis (FWE)
# Six possible parameters for reduction
# QDR / FWE / 29 May 2019

# Modified 26 June 2019: add eutrophication potential - and correct previous errors with lower and upper bounds, and proportions
# Modified 20 June 2019: split up cost parameter by proportional size of the sectors.
# Modified 17 June 2019: Use refed numbers for the cost curve part.

# 6 reduction areas: agriculture, processing, retail, institutional consumption, food service consumption, household consumption

# Load data ---------------------------------------------------------------

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
eval_f_eeio <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, W0_sectors_final, Wu_sectors_final, B_sectors_final, nu_sectors_final, p_sectors_final, proportion_food_sectors, sector_stage_codes, final_demand_sector_codes, Ctotal) {
  # Find reduction rates given costs x
  # We need a different W0, Wu, B, and possibly nu value for each element in the waste rate vector.
  r_sectors <- get_reduction_rates(x = x, W0_sectors = W0_sectors, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = p_sectors, W0_sectors_final = W0_sectors_final, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = p_sectors_final, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes)
  
  # Factors by which to multiply intermediate and final demand given the waste reductions calculated from costs x
  # Also multiply this by the proportion food for each of the sectors.
  intermediate_demand_change_factors <- demand_change_fn(W0 = W0_sectors, r = r_sectors$intermediate, p = p_sectors * proportion_food_sectors)
  final_demand_change_factors <- demand_change_fn(W0 = W0_sectors, r = r_sectors$final, p = p_sectors_final * proportion_food_sectors)
    
  # define the scenario values given x. 
  res <- get_eeio_result(c_factor = intermediate_demand_change_factors,
                         c_names = sector_long_names,
                         r_factor = final_demand_change_factors,
                         r_names = sector_short_names)
  # Return the impact value to be minimized.
  res$value[res$impact_category == category]
}

# Another version of constraint function that just returns the sum of the x values. For Rsolnp package.
eval_eq_total <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, W0_sectors_final, Wu_sectors_final, B_sectors_final, nu_sectors_final, p_sectors_final, proportion_food_sectors, sector_stage_codes, final_demand_sector_codes, Ctotal) {
  return(sum(x))
}


# Run "grid" scenario 6 ways ----------------------------------------------

# Use 5 levels of reduction for each sector. 0%, 25%, 50%, 75%, 100%
# 5 levels of reduction in 6 sectors = 5^6 scenarios = 15625

rate_levels <- c(0, 0.25, 0.5, 0.75, 1)
reduction_rate_grid <- expand.grid(L1 = rate_levels, L2 = rate_levels, L3 = rate_levels, L4a = rate_levels, L4b = rate_levels, L5 = rate_levels)

# Create list from grid
reduction_rate_grid_list <- setNames(split(reduction_rate_grid, seq(nrow(reduction_rate_grid))), rownames(reduction_rate_grid))

# Reductions 1-3 modify inputs to sector columns in classes 1-3.
# Reductions 4a and 4b modify inputs to sector columns and reduction to the corresponding rows of PCE final demand (4a and 4b respectively)
# Reduction 5 does not modify any inputs to sector columns. It modifies PCE final demand in classes 1-3 but not 4a or 4b.

# For each combination scenario, put together a vector of demand reduction factors for intermediate demand with values and names equal to the column names of the DRC matrix
# Also put together a vector of demand reduction factors for final demand with values and names equal to the row names of the DRC matrix. This will modify the PCE column of final demand.

# Run in parallel with mcmapply
get_reduction <- function(reduction_by_stage, scenario_id) {
  intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[sector_stage_codes], naics_foodsystem$proportion_food))
  final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[final_demand_sector_codes], naics_foodsystem$proportion_food))
  get_eeio_result(c_factor = intermediate_demand_change_factors,
                  c_names = sector_long_names,
                  r_factor = final_demand_change_factors,
                  r_names = sector_short_names,
                  i = scenario_id)
}

library(parallel)
eeio_result_grid <- mcmapply(get_reduction, reduction_by_stage = reduction_rate_grid_list, scenario_id = 1:length(reduction_rate_grid_list), mc.cores = 4, SIMPLIFY = FALSE)
eeio_result_grid_df <- bind_rows(eeio_result_grid)

# Put output into data frame and write to CSV.
eeio_result_grid_df <- cbind(reduction_rate_grid[rep(1:nrow(reduction_rate_grid), each = 21), ], eeio_result_grid_df)

# To see if results are plausible, look at results where a single sector is reduced by 100%
oneby100 <- rowSums(eeio_result_grid_df[,1:6]) == 1 & rowSums(eeio_result_grid_df[,1:6] > 0) == 1
result_oneby100 <- eeio_result_grid_df[rowSums(eeio_result_grid_df[,1:6]) == 0 | oneby100, ]  

result_oneby100 %>% filter(grepl('co2', impact_category))

write.csv(eeio_result_grid_df, file = '/nfs/qread-data/scenario_results/sixstage_scenario_grid_lcia_results.csv', row.names = FALSE)

# Do optimization for different impact categories -------------------------

library(Rsolnp)

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

# B: Slope
b <- function(W0, W1, Wu, C1) log(2 * (W0 - Wu)/(W1 - Wu)) / C1

B_sectors <- b(W0 = baseline_waste_rate, W1 = W1_sectors, Wu = Wu_sectors, C1 = C1_sectors)
B_sectors_final <- b(W0 = baseline_waste_rate, W1 = W1_sectors_final, Wu = Wu_sectors_final, C1 = C1_sectors_final)

# nu: Currently set to 1 for all.
nu_sectors <- rep(1, length(B_sectors))
nu_sectors_final <- rep(1, length(B_sectors_final))


# Write all the parameter values to a CSV for later plotting
sectorpars <- data.frame(sector_code = naics_foodsystem$BEA_389_code,
                         sector_name = naics_foodsystem$BEA_389_def,
                         stage = naics_foodsystem$stage,
                         stage_code = sector_stage_codes,
                         stage_code_final = final_demand_sector_codes,
                         Wu = Wu_sectors,
                         Wu_final = Wu_sectors_final,
                         W0 = baseline_waste_rate,
                         B = B_sectors,
                         B_final = B_sectors_final)
write.csv(sectorpars, file.path(fp_output, 'sector_parameters.csv'), row.names = FALSE)

# Optimization
# Initial values: 1/6 of money spent on each FSC stage, and lower and upper bounds are just there to keep everything >= 0.

# Names of categories to optimize over
ghg_name <- "impact potential/gcc/kg co2 eq" 
land_name <- "resource use/land/m2*yr"
water_name <- "resource use/watr/m3"
energy_name <- "resource use/enrg/mj"
eutr_name <- "impact potential/eutr/kg n eq"

# Optimize across each of the 4 categories and for each of several values of Ctotal
Ctotal_vec <- c(500, 1000, 2000, 5000)

optim_ghg <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = naics_foodsystem$proportion_food, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))

optim_land <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = land_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = naics_foodsystem$proportion_food, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))

optim_water <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = water_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = naics_foodsystem$proportion_food, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))

optim_energy <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = energy_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = naics_foodsystem$proportion_food, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))

optim_eutr <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = eutr_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = naics_foodsystem$proportion_food, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))


# Process output and write.
stage_full_names <- c('production', 'processing', 'retail', 'consumption: food service', 'consumption: institutional', 'consumption: household')
makeoptimdf <- function(o) map2_dfr(o, Ctotal_vec, ~ data.frame(total_cost = .y, stage = factor(stage_full_names, levels = stage_full_names), cost = .x$pars))

optimal_df_ghg <- makeoptimdf(optim_ghg)
optimal_df_land <- makeoptimdf(optim_land)
optimal_df_water <- makeoptimdf(optim_water)
optimal_df_energy <- makeoptimdf(optim_energy)
optimal_df_eutr <- makeoptimdf(optim_eutr)

optimal_df_all <- map2_dfr(c('GHG','land','water','energy','eutrophication'), list(optimal_df_ghg, optimal_df_land, optimal_df_water, optimal_df_energy, optimal_df_eutr), ~ cbind(category = .x, .y)) %>%
  mutate(cost = round(cost))

write.csv(optimal_df_all, '/nfs/qread-data/scenario_results/sixstage_scenario_opt_results.csv', row.names = FALSE)

# Also get the actual value that is minimized from each optimization.
makeoptimvaldf <- function(o) map2_dfr(o, Ctotal_vec, ~ data.frame(total_cost = .y, value = .x$values[length(.x$values)]))

optimal_val_ghg <- makeoptimvaldf(optim_ghg)
optimal_val_land <- makeoptimvaldf(optim_land)
optimal_val_water <- makeoptimvaldf(optim_water)
optimal_val_energy <- makeoptimvaldf(optim_energy)
optimal_val_eutr <- makeoptimvaldf(optim_eutr)

optimal_value_all <- map2_dfr(c('GHG','land','water','energy','eutrophication'), list(optimal_val_ghg, optimal_val_land, optimal_val_water, optimal_val_energy, optimal_val_eutr), ~ cbind(category = .x, .y))

write.csv(optimal_value_all, '/nfs/qread-data/scenario_results/sixstage_scenario_opt_values.csv', row.names = FALSE)
