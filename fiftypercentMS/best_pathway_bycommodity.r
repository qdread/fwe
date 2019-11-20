# Scenario analysis done separately for each commodity
# QDR / FWE / 23 Sep 2019

# 5 environmental goods: energy, water, GHG, land, eutrophication
# ~13 commodities: the 11 FAO groups plus sugars and beverages (some are not present in all reduction areas)
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

### Get waste rates by sector x commodity
waste_rate_bycommodityxsector <- waste_rate_bysector * fao_category_weights

# Function definitions ----------------------------------------------------

# Function to get change in demand given original waste rate W0, reduction rate r, and proportion of the sector's output that is food p
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# Function to evaluate USEEIO for a single scenario, specifying the waste rate for the commodity we want.
evaluate_scenario <- function(reduction_by_stage, baseline_waste_rate, scenario_id) {
  intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[sector_stage_codes], naics_foodsystem$proportion_food))
  final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[final_demand_sector_codes], naics_foodsystem$proportion_food))
  intermediate_demand_change_factors[is.na(intermediate_demand_change_factors)] <- 1
  final_demand_change_factors[is.na(final_demand_change_factors)] <- 1
  get_eeio_result(c_factor = intermediate_demand_change_factors,
                  c_names = sector_long_names,
                  r_factor = final_demand_change_factors,
                  r_names = sector_short_names,
                  i = scenario_id)
}

# Set up data to parallelize ----------------------------------------------

# This will be run for 5 environmental goods x 13 commodities. But each combination only needs to run 20 things so it will only take a couple of minutes each.
# Can run with just a few cores.
# We will use the reduction rate of 50%. The results should be identical regardless of which is used.

# Environmental goods to minimize
categories <- c("impact potential/gcc/kg co2 eq", "resource use/land/m2*yr", "resource use/watr/m3", "resource use/enrg/mj", "impact potential/eutr/kg n eq")

# Commodities to target
commodities <- names(fao_category_weights)

# All combinations of category x commodity
scenarios <- expand.grid(category = categories, commodity = commodities, stringsAsFactors = FALSE)

# Define a function to run for each of the 65 combinations

find_best_pathway <- function(category, commodity) {
  # Find the waste rates for the commodity being evaluated
  commodity_waste_rates <- waste_rate_bycommodityxsector[, commodity]
  
  # Find the stages across which we will optimize (many will have 6 but some that aren't in agriculture stage will only have 5)
  stages <- c('L2','L3','L4a','L4b','L5')
  if (!is.na(commodity_waste_rates[1])) stages <- c('L1', stages)
  
  # Initialize best pathway and other structures to hold data
  best_pathway <- c()
  best_pathway_impacts <- list()
  reductions <- setNames(rep(0, 6), c('L1', 'L2', 'L3', 'L4a', 'L4b', 'L5'))
  
  # Each time the iteration runs, we select one stage from the remaining not yet chosen stages and add it to the already chosen stages
  # Then run the scenario and see which stage minimizes the environmental impact in the chosen good
  # This is still done even when there is only one stage left since we want to know the impacts when the one final stage is reduced
  for (i in 1:length(stages)) {
    # Print a progress message
    message('Evaluating stage ', i, ' for ', commodity, ' minimizing ', category)
    
    # Define each of the remaining scenarios
    candidate_scenarios <- sapply(stages, function(x) {
      to_reduce <- c(best_pathway, x)
      reductions[to_reduce] <- 0.5
      return(reductions)
    }, simplify = FALSE)
    
    # Evaluate each scenario for the given commodity only (just serially, not in parallel, since there are at most 6)
    scenario_results <- map(candidate_scenarios, evaluate_scenario, baseline_waste_rate = commodity_waste_rates, scenario_id = paste(commodity, gsub('[^a-z]', '', category), i, sep = '_'))
    
    # Identify which scenario minimizes the environmental good in question, and append it to best_pathway
    impact_values <- map_dbl(scenario_results, ~ .x$value[.x$impact_category == category])
    best_stage <- which.min(impact_values)
    best_pathway <- c(best_pathway, stages[best_stage])
    
    # Remove it from the remaining candidate stages
    stages <- stages[!stages %in% best_pathway]
    
    # Write the best scenario results to the list of outputs
    best_pathway_impacts[[i]] <- scenario_results[[best_stage]]
  }
  
  # Return the results
  return(list(best_pathway = best_pathway, best_pathway_impacts = best_pathway_impacts))
}


# Run in parallel across all combinations ---------------------------------

library(parallel)
results_by_commodity <- mcmapply(find_best_pathway, category = scenarios$category, commodity = scenarios$commodity, mc.cores = 4, SIMPLIFY = FALSE)
results_by_commodity <- tibble(category = scenarios$category, 
                               commodity = scenarios$commodity, 
                               best_pathway = map(results_by_commodity, 'best_pathway'), 
                               impacts = map(results_by_commodity, 'best_pathway_impacts'))

# Write a tibble to .RData and process it in another script
save(results_by_commodity, file = '/nfs/qread-data/scenario_results/pathways_by_commodity_output.RData')
