# Draft code for new scenario analysis (FWE)
# Six possible parameters for reduction
# QDR / FWE / 29 May 2019

# reduction areas: agriculture, processing, retail, institutional consumption, food service consumption, household consumption


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



# Run grid with furrr
# plan(multiprocess(workers = 8))
# 
# eeio_result_grid <- future_imap_dfr(reduction_rate_grid_list, function(reduction_by_stage, scenario_id) {
#   intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[sector_stage_codes], naics_foodsystem$proportion_food))
#   final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[final_demand_sector_codes], naics_foodsystem$proportion_food))
#   get_eeio_result(c_factor = intermediate_demand_change_factors,
#                   c_names = sector_long_names,
#                   r_factor = final_demand_change_factors,
#                   r_names = sector_short_names,
#                   i = scenario_id)
# }, .progress = TRUE)

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
#short_list <- reduction_rate_grid_list[1:40]
eeio_result_grid <- mcmapply(get_reduction, reduction_by_stage = reduction_rate_grid_list, scenario_id = 1:length(reduction_rate_grid_list), mc.cores = 4, SIMPLIFY = FALSE)
eeio_result_grid_df <- bind_rows(eeio_result_grid)

# # Test: run serial
# eeio_result_grid_list <- list()
# for (i in 1:length(reduction_rate_grid_list)) {
#   reduction_by_stage <- reduction_rate_grid_list[[i]]
#   intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[sector_stage_codes], naics_foodsystem$proportion_food))
#   final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[final_demand_sector_codes], naics_foodsystem$proportion_food))
#   eeio_result_grid_list[[i]] <- get_eeio_result(c_factor = intermediate_demand_change_factors,
#                   c_names = sector_long_names,
#                   r_factor = final_demand_change_factors,
#                   r_names = sector_short_names,
#                   i = names(reduction_rate_grid_list)[i])
# }
# 
# eeio_result_grid_df <- cbind(reduction_rate_grid[rep(1:nrow(reduction_rate_grid), each = nrow(eeio_result_grid_list[[1]])), ], bind_rows(eeio_result_grid_list))


# Put output into data frame and write to CSV.
eeio_result_grid_df <- cbind(reduction_rate_grid[rep(1:nrow(reduction_rate_grid), each = 21), ], eeio_result_grid_df)

# To see if results are plausible, look at results where a single sector is reduced by 100%
oneby100 <- rowSums(eeio_result_grid_df[,1:6]) == 1 & rowSums(eeio_result_grid_df[,1:6] > 0) == 1
result_oneby100 <- eeio_result_grid_df[rowSums(eeio_result_grid_df[,1:6]) == 0 | oneby100, ]  

result_oneby100 %>% filter(grepl('co2', impact_category))

write.csv(eeio_result_grid_df, file = '/nfs/qread-data/scenario_results/sixstage_scenario_grid_lcia_results.csv', row.names = FALSE)

# Do optimization for different impact categories -------------------------

library(Rsolnp)

# For now, use fake abatement cost curves to test and make sure it works.

# Fake parameters to test if procedure works:
# fake unavoidable waste rate -- assume 25% of food waste is unavoidable
fake_unavoidable_waste_rate <- baseline_waste_rate * 0.25

# Parameters by stage
# Assume faster returns on investment for level 1 and level 2
# Assume higher startup costs for level 1, intermediate for level 2, none for level 3
B_stages <- c(L1 = 0.005, L2 = 0.005, L3 = 0.002, L4a = 0.003, L4b = 0.003, L5 = 0.007)
nu_stages <- c(L1 = 0.1, L2 = 0.2, L3 = 1, L4a = 0.9, L4b = 0.9, L5 = 0.05)

B_sectors <- B_stages[sector_stage_codes]
nu_sectors <- nu_stages[sector_stage_codes]

B_sectors_final <- B_stages[final_demand_sector_codes]
nu_sectors_final <- nu_stages[final_demand_sector_codes]

# Find the proportion of waste reduction dollars allocated to each sector within each stage of the food supply chain
# Use the baseline values for gross output from each sector to get the weights. (this is T008 in the make table)
gross_outputs <- M[sector_short_names, 'T008']
gross_outputs_by_stage <- tapply(gross_outputs, sector_stage_codes, sum)
proportion_gross_outputs <- gross_outputs / gross_outputs_by_stage[sector_stage_codes]

# Also do this proportion for the final ones.
gross_outputs_by_stage_final <- tapply(gross_outputs, final_demand_sector_codes, sum)
proportion_gross_outputs_final <- gross_outputs / gross_outputs_by_stage_final[final_demand_sector_codes]

# Constraint: total cost - pulled out of thin air.
Ctotal <- 1000


# Optimization
# Initial values: 1/6 of money spent on each FSC stage, and lower and upper bounds are just there to keep everything >= 0.
x0 <- rep(Ctotal/6, 6)
lb <- rep(0, 6)
ub <- rep(Ctotal, 6)

# Names of categories to optimize over
ghg_name <- "impact potential/gcc/kg co2 eq" 
land_name <- "resource use/land/m2*yr"
water_name <- "resource use/watr/m3"
energy_name <- "resource use/enrg/mj"

# Test. (looks like the 6 parameters take longer to evaluate than the 3 situation)
res_test <- solnp(pars = x0, fun = eval_f_eeio, eqfun = eval_eq_total, eqB = Ctotal, LB = lb, UB = ub, category = ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = fake_unavoidable_waste_rate, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = Ctotal)
### works.

# Optimize across each of the 4 categories and for each of several values of Ctotal
Ctotal_vec <- c(500, 1000, 2000, 5000)

optim_ghg <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = lb, UB = ub, category = ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = fake_unavoidable_waste_rate, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))

optim_land <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = lb, UB = ub, category = land_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = fake_unavoidable_waste_rate, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))

optim_water <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = lb, UB = ub, category = water_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = fake_unavoidable_waste_rate, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))

optim_energy <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = lb, UB = ub, category = energy_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = fake_unavoidable_waste_rate, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x))

# Process output and write.
stage_full_names <- c('production', 'processing', 'retail', 'consumption: food service', 'consumption: institutional', 'consumption: household')
makeoptimdf <- function(o) map2_dfr(o, Ctotal_vec, ~ data.frame(total_cost = .y, stage = factor(stage_full_names, levels = stage_full_names), cost = .x$pars))

optimal_df_ghg <- makeoptimdf(optim_ghg)
optimal_df_land <- makeoptimdf(optim_land)
optimal_df_water <- makeoptimdf(optim_water)
optimal_df_energy <- makeoptimdf(optim_energy)

optimal_df_all <- map2_dfr(c('GHG','land','water','energy'), list(optimal_df_ghg, optimal_df_land, optimal_df_water, optimal_df_energy), ~ cbind(category = .x, .y)) %>%
  mutate(cost = round(cost))

write.csv(optimal_df_all, '/nfs/qread-data/scenario_results/sixstage_scenario_fake_opt_results.csv', row.names = FALSE)
