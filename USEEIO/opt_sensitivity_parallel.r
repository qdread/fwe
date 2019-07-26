# Sensitivity analysis of optimization
# QDR / FWE / 26 June 2019

# Modified 30 June 2019: Use triangular distributions on the underlying quantities from ReFed, then calc the parameters from those.
# Modified 27 June 2019: Create unique ID for each model build so that the parallel jobs do not conflict with each other.

# Load data ---------------------------------------------------------------

task <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # Split into n tasks. Each will take 1 node with 8 cores so 8*n processes simultaneously run.
n_tasks <- 5

library(foreach)
library(doParallel)
library(Rsolnp)
library(EnvStats)
library(truncdist)
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

# Modified function to get EEIO result
get_eeio_result <- function(c_factor, r_factor, c_names, r_names, id = 'no_name', crosswalk = naics_foodsystem) {
  
  # Build USEEIO with specifications for modifying intermediate and final demand.
  build_USEEIO(outputfolder = file.path(model_build_path, paste0('scenario_', id)),
               model = paste0('scenario_', id),
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
  all_final_demand <- read.csv(file.path(model_build_path, paste0('scenario_', id), paste0('scenario_', id, '_FinalDemand.csv')), stringsAsFactors = FALSE) %>%
    left_join(crosswalk, by = c('BEA_389_code', 'BEA_389_def')) %>% 
    filter(!is.na(proportion_food)) %>%
    left_join(all_codes, by = c('BEA_389_code' = 'sector_code_uppercase'))
  # Convert demand vector to separate list of codes and values
  final_demand_list <- with(all_final_demand, list(codes = as.list(sector_desc_drc), values = as.list(X2012_US_Consumption * proportion_food)))
  # Run the model!
  eeio_result <- eeio_lcia(paste0('scenario_', id), final_demand_list$values, final_demand_list$codes) 
  eeio_result <- data.frame(scenario = id, impact_category = rownames(eeio_result), value = eeio_result$Total, stringsAsFactors = FALSE)
  # Delete the intermediate files (entire model build folder) 
  unlink(file.path(model_build_path, paste0('scenario_', id)), recursive = TRUE)
  
  return(eeio_result)
}



# Evaluation function used for optimization (takes in costs and returns environmental impacts given waste reduction scenario based on those costs)
# Modified for the parallel sensitivity jobs to give unique text string IDs each time it is run.
eval_f_eeio <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, W0_sectors_final, Wu_sectors_final, B_sectors_final, nu_sectors_final, p_sectors_final, proportion_food_sectors, sector_stage_codes, final_demand_sector_codes, Ctotal, draw_id) {
  # Find reduction rates given costs x
  # We need a different W0, Wu, B, and possibly nu value for each element in the waste rate vector.
  r_sectors <- get_reduction_rates(x = x, W0_sectors = W0_sectors, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = p_sectors, W0_sectors_final = W0_sectors_final, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = p_sectors_final, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes)
  
  # Factors by which to multiply intermediate and final demand given the waste reductions calculated from costs x
  intermediate_demand_change_factors <- demand_change_fn(W0 = W0_sectors, r = r_sectors$intermediate, p = p_sectors * proportion_food_sectors)
  final_demand_change_factors <- demand_change_fn(W0 = W0_sectors, r = r_sectors$final, p = p_sectors_final * proportion_food_sectors)
  
  # define the scenario values given x. 
  
  res <- get_eeio_result(c_factor = intermediate_demand_change_factors,
                         c_names = sector_long_names,
                         r_factor = final_demand_change_factors,
                         r_names = sector_short_names,
                         id = draw_id,
                         crosswalk = naics_foodsystem %>% mutate(proportion_food = !!proportion_food_sectors))
  # Return the impact value to be minimized.
  res$value[res$impact_category == category]
}

# Another version of constraint function that just returns the sum of the x values. For Rsolnp package.
eval_eq_total <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, W0_sectors_final, Wu_sectors_final, B_sectors_final, nu_sectors_final, p_sectors_final, proportion_food_sectors, sector_stage_codes, final_demand_sector_codes, Ctotal, draw_id) {
  return(sum(x))
}

# Function to draw from beta distribution and create a new vector
beta_draw <- function(p, f = 100) rbeta(n = length(p), shape1 = f * p, shape2 = f * (1 - p))

# Function to draw from truncated beta distribution and create a new vector
trunc_beta_draw <- function(p, upper, f = 100) sapply(1:length(p), function(i) rtrunc(n = 1, 'beta', a = 0, b = upper[i], shape1 = f * p[i], shape2 = f * (1 - p[i])))

# Function to draw from triangular distribution and create a new vector
triangle_draw <- function(x, f = 0.5) rtri(n = length(x), min = (1 - f) * x, max = (1 + f) * x, mode = x)

trunc_triangle_draw <- function(x, upper, f = 0.5) sapply(1:length(x), function(i) rtrunc(n = 1, 'tri', a = 0, b = upper[i], min = (1 - f) * x[i], max = (1 + f) * x[i], mode = x[i]))

# Function to generate a sampling draw for the baseline waste rates.
baseline_rate_table_draw <- function(w, f = 100) {
  res <- apply(w, 2, function(p) rbeta(n = length(p), shape1 = f * p, shape2 = f * (1 - p)))
  res[!is.finite(res)] <- NA
  res
}

# Random draws of parameters ----------------------------------------------

n_draws <- 50

# Determine minimum and maximum index to do in this task
idxmin <- seq(1,n_draws,by=n_draws/n_tasks)[task]
idxmax <- seq(n_draws/n_tasks,n_draws,by=n_draws/n_tasks)[task]

uncertainty_factor <- 100
triangle_width <- 0.2 # Relative to the original value (edited 1 July - narrower so the result makes more sense)
set.seed(777)

# Get parameters for abatement curves from Refed data.
refed_params <- read.csv(file.path(fp_crosswalks, 'refed_testvalues.csv'), stringsAsFactors = FALSE)

# Draw n_draws sets of refed params. Wu and W1 are all relative to baseline waste rate.
refed_params_list <- replicate(n_draws, refed_params %>%
                                 mutate(net = triangle_draw(net, triangle_width),
                                        addressable = trunc_triangle_draw(addressable, net, triangle_width),
                                        diversion.potential = trunc_triangle_draw(diversion.potential, addressable, triangle_width),
                                        cost = triangle_draw(cost, triangle_width),
                                        Wu = 1 - addressable/net,
                                        W1 = 1 - diversion.potential/net,
                                        C1 = cost,
                                        stage_code = c('L1', 'L2', 'L3', 'L4a', 'L4b', 'L5')),
                               simplify = FALSE)

# Draw n_draws sets of baseline waste rate params.
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
proportion_food_list <- replicate(n_draws, beta_draw(p = naics_foodsystem$proportion_food, f = uncertainty_factor), simplify = FALSE)


# Generate final parameter lists from random draws ------------------------

waste_rate_bysector_list <- map(faopct_list, ~ t(.x[, naics_foodsystem$stage_code]))
fao_category_weights <- naics_foodsystem %>% select(cereals:beverages) # Do not bother to do sensitivity analysis for this.
W0_sectors_list <- map(waste_rate_bysector_list, ~ rowSums(.x * fao_category_weights, na.rm = TRUE) / rowSums(fao_category_weights))

# Wu: Unavoidable waste rate 
Wu_sectors_list <- map2(refed_params_list, W0_sectors_list, ~ .y * .x$Wu[match(sector_stage_codes, .x$stage_code)])
Wu_sectors_final_list <- map2(refed_params_list, W0_sectors_list, ~ .y * .x$Wu[match(final_demand_sector_codes, .x$stage_code)])

# W1: Diverted waste amount at C1
W1_sectors_list <- map2(refed_params_list, W0_sectors_list, ~ .y * .x$W1[match(sector_stage_codes, .x$stage_code)])
W1_sectors_final_list <- map2(refed_params_list, W0_sectors_list, ~ .y * .x$W1[match(final_demand_sector_codes, .x$stage_code)])

# Find the proportion of waste reduction dollars allocated to each sector within each stage of the food supply chain
# Use the baseline values for gross output from each sector to get the weights. (this is T008 in the make table)
# Modification 22 July 2019: Use proportion food for each one.
gross_outputs <- M[sector_short_names, 'T008']
gross_outputs_list <- map(proportion_food_list, ~ .x * gross_outputs)
gross_outputs_by_stage_list <- map(gross_outputs_list, ~ tapply(.x, sector_stage_codes, sum))
proportion_gross_outputs_list <- map2(gross_outputs_list, gross_outputs_by_stage_list, ~ .x / .y[sector_stage_codes])

# Also do this proportion for the final ones.
gross_outputs_by_stage_final_list <- map(gross_outputs_list, ~ tapply(.x, final_demand_sector_codes, sum))
proportion_gross_outputs_final_list <- map2(gross_outputs_list, gross_outputs_by_stage_final_list, ~ .x / .y[final_demand_sector_codes])


# C1: Cost to achieve W1
C1_sectors_list <- map2(refed_params_list, proportion_gross_outputs_list, ~ .x$C1[match(sector_stage_codes, .x$stage_code)] * .y)
C1_sectors_final_list <- map2(refed_params_list, proportion_gross_outputs_final_list, ~ .x$C1[match(sector_stage_codes, .x$stage_code)] * .y)

# B: Slope
b <- function(W0, W1, Wu, C1) log(2 * (W0 - Wu)/(W1 - Wu)) / C1

B_sectors_list <- map(1:n_draws, ~ b(W0 = W0_sectors_list[[.x]], W1 = W1_sectors_list[[.x]], Wu = Wu_sectors_list[[.x]], C1 = C1_sectors_list[[.x]]))
B_sectors_final_list <- map(1:n_draws, ~ b(W0 = W0_sectors_list[[.x]], W1 = W1_sectors_final_list[[.x]], Wu = Wu_sectors_final_list[[.x]], C1 = C1_sectors_final_list[[.x]]))

# nu: Currently set to 1 for all. (no need to make list)
nu_sectors <- rep(1, length(B_sectors_list[[1]]))
nu_sectors_final <- rep(1, length(B_sectors_final_list[[1]]))


# Run optimization for all parameter sets ---------------------------------

# Also repeat for all environmental goods and all values of Ctotal.

# Names of categories to optimize over
ghg_name <- "impact potential/gcc/kg co2 eq" 
land_name <- "resource use/land/m2*yr"
water_name <- "resource use/watr/m3"
energy_name <- "resource use/enrg/mj"
eutr_name <- "impact potential/eutr/kg n eq"

# Optimize across each of the 4 categories and for each of several values of Ctotal
Ctotal_vec <- c(500, 1000, 2000, 5000)

registerDoParallel(cores = n_cores)

optim_list <- foreach(i = idxmin:idxmax) %dopar% {
  W0_sectors <- W0_sectors_list[[i]]
  Wu_sectors <- Wu_sectors_list[[i]]
  B_sectors <- B_sectors_list[[i]]
  Wu_sectors_final <- Wu_sectors_final_list[[i]]
  B_sectors_final <- B_sectors_final_list[[i]]
  proportion_food_sectors <- proportion_food_list[[i]]
  proportion_gross_outputs <- proportion_gross_outputs_list[[i]]
  proportion_gross_outputs_final <- proportion_gross_outputs_final_list[[i]]
  
  optim_ghg <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = ghg_name, W0_sectors = W0_sectors, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = proportion_food_sectors, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x, draw_id = paste('ghg',.x,i,sep='_')))
  
  optim_land <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = land_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = proportion_food_sectors, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x, draw_id = paste('land',.x,i,sep='_')))
  
  optim_water <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = water_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = proportion_food_sectors, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x, draw_id = paste('water',.x,i,sep='_')))
  
  optim_energy <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = energy_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = proportion_food_sectors, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x, draw_id = paste('energy',.x,i,sep='_')))
  
  optim_eutr <- map(Ctotal_vec, ~ solnp(pars = rep(.x/6, 6), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = rep(0, 6), UB = rep(.x, 6), category = eutr_name, W0_sectors = baseline_waste_rate, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, W0_sectors_final = baseline_waste_rate, Wu_sectors_final = Wu_sectors_final, B_sectors_final = B_sectors_final, nu_sectors_final = nu_sectors_final, p_sectors_final = proportion_gross_outputs_final, proportion_food_sectors = proportion_food_sectors, sector_stage_codes = sector_stage_codes, final_demand_sector_codes = final_demand_sector_codes, Ctotal = .x, draw_id = paste('eutr',.x,i,sep='_')))
  
  message('Iteration ', i, ' done')
  
  list(optim_ghg, optim_land, optim_water, optim_energy, optim_eutr)
  
}

save(optim_list, file = file.path(fp_output, paste0('optim_sens_list_', task, '.RData')))
