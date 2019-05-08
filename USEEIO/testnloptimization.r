# Optimization of a single environmental impact variable (minimize impact), given a fixed amount to spend on waste reduction
# For now, use fake numbers and cost curves for the different stages

# Problem:
# evaluation function: environmental impact y = f(L1, L2, L3) = f(C1, C2, C3)
# waste rate L at each FSC stage L1 = f(C1); L2 = f(C2); L3 = f(C3). These can be decreasing exponential curves such that L1 asymptotically approaches "unavoidable" waste level as C1 increases.
# budget constraint C1 + C2 + C3 = Ctotal, or C1 + C2 + C3 - Ctotal = 0

# Gradient of environmental impact function can be calculated with numerical approximation.

# Specification:

# x is vector of the three dollar amounts spent on waste reduction at each of the three stages.


# Function definitions ----------------------------------------------------

# Function to get change in demand given original waste rate W0, reduction rate r, and proportion of the sector's output that is food p
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# Exponential or Richards curves for new waste rate given cost x, old waste rate W0, unavoidable waste rate Wu, and parameters B and nu
waste_rate_by_cost <- function(x, W0, Wu, B, nu) {
  # If nu = 1 this becomes the exponential curve
  A <- (W0 - (2 ^ (-1/nu)) * Wu) / (1 - 2 ^ (-1/nu))
  A + (Wu - A)/((1 + exp(-B*x))^(1/nu))
}

# function to calculate the waste rates for all sectors, given the costs for the 3 stages, and the parameters described above
get_reduction_rates <- function(x, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, sector_stage_codes) {
  x <- setNames(x, c('L1', 'L2', 'L3'))
  allocated_costs <- x[sector_stage_codes] * p_sectors
  Wnew_sectors <- waste_rate_by_cost(x = allocated_costs, W0 = W0_sectors, Wu = Wu_sectors, B = B_sectors, nu = nu_sectors)
  r_sectors <- 1 - Wnew_sectors / W0_sectors 
}


# Evaluation function contains the name of the single impact category being minimized.
# Argument x is the amount of money being spent on waste reduction in each of the 3 stages, which must sum to the total cost.
# This should work for both NlcOptim and nloptr
eval_f_eeio <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, sector_stage_codes, Ctotal) {
  # Find reduction rates given costs x
  # We need a different W0, Wu, B, and possibly nu value for each element in the waste rate vector.
  r_sectors <- get_reduction_rates(x = x, W0_sectors = W0_sectors, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = p_sectors, sector_stage_codes = sector_stage_codes)
  
  # Factors by which to multiply intermediate and final demand given the waste reductions calculated from costs x
  demand_change_factors <- demand_change_fn(W0 = W0_sectors, r = r_sectors, p = p_sectors)
  
  # define the scenario values given x. 
  res <- get_eeio_result(demand_change_factors)
  # Return the impact value to be minimized.
  res$value[res$impact_category == category]
}

# Gradient of evaluation function
# We must numerically evaluate the gradient of the evaluation function because of the eeio routine embedded in it.
eval_grad_f_eeio <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, sector_stage_codes, Ctotal) {
  grad(eval_f_eeio, x, method = 'Richardson', category = category, W0_sectors = W0_sectors, Wu_sectors = Wu_sectors, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = p_sectors, sector_stage_codes = sector_stage_codes)
}

# Constraints: just an equality constraint such that the total amount of $ spent on food waste reduction is a fixed amount.
# Note that all objective and constraint functions must take the same arguments.
# Works for nloptr package
eval_eq_totalcost <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, sector_stage_codes, Ctotal) {
  constr <- c(x[1] + x[2] + x[3] - Ctotal)
  gradient <- c(1, 1, 1)
  return(list(constraints = constr, jacobian = gradient))
}

# Another version of the constraint function that doesn't include a gradient - just a ceq vector. This is for NlcOptim package
eval_eq_totalcost_ceq <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, sector_stage_codes, Ctotal) {
  constr <- c(x[1] + x[2] + x[3] - Ctotal)
  return(list(ceq = constr, c = NULL))
}

# Another version of constraint function that just returns the sum of the x values. For Rsolnp package.
eval_eq_total <- function(x, category, W0_sectors, Wu_sectors, B_sectors, nu_sectors, p_sectors, sector_stage_codes, Ctotal) {
  return(sum(x))
}


# Create semi-fake data to use in optimization ----------------------------

library(nloptr)
library(numDeriv)
library(NlcOptim)
library(Rsolnp)

fp_github <- ifelse(dir.exists('~/Documents/GitHub'), '~/Documents/GitHub/fwe', '/research-home/qread/fwe')
source(file.path(fp_github, 'USEEIO/load_scenario_data.r'))

# fake unavoidable waste rate -- assume 25% of food waste is unavoidable
fake_unavoidable_waste_rate <- baseline_waste_rate * 0.25

# B and nu by stage
# Assume faster returns on investment for level 2
# Assume higher startup costs for level 1
B_stages <- c(L1 = 0.002, L2 = 0.005, L3 = 0.002)
nu_stages <- c(L1 = 0.1, L2 = 0.5, L3 = 1)

B_sectors <- B_stages[naics_foodsystem$stage_code]
nu_sectors <- nu_stages[naics_foodsystem$stage_code]

# Find the proportion of waste reduction dollars allocated to each sector within each stage of the food supply chain
# Use the baseline values for gross output from each sector to get the weights. (this is T008 in the make table)

gross_outputs <- M[naics_foodsystem$BEA_389_code, 'T008']
gross_outputs_by_stage <- tapply(gross_outputs, naics_foodsystem$stage_code, sum)
proportion_gross_outputs <- gross_outputs / gross_outputs_by_stage[naics_foodsystem$stage_code]

# Constraint: total cost - pulled out of thin air.
Ctotal <- 1000


# Run optimization --------------------------------------------------------

# Initial values: 1/3 of money spent on each FSC stage, and lower and upper bounds are just there to keep everything >= 0.
x0 <- rep(Ctotal/3, 3)
# Try a different initial value
x0 <- c(Ctotal/4, Ctotal/2, Ctotal/4)

lb <- rep(0, 3)
ub <- rep(Ctotal, 3)
ghg_name <- "impact potential/gcc/kg co2 eq" # GHG emissions are the category to minimize for now

# Options
local_opts <- list(algorithm = 'NLOPT_LD_MMA', xtol_rel = 1e-7)
opts <- list(algorithm = 'NLOPT_LD_AUGLAG', xtol_rel = 1e-7, maxeval = 1000, print_level = 3, local_opts = local_opts)

# Test to see if the evaluation function returns something at the initial values
# These work but requires a couple of minutes to evaluate the gradient.
y_test <- eval_f_eeio(x=x0, category=ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, sector_stage_codes = naics_foodsystem$stage_code)
grad_test <- eval_grad_f_eeio(x = x0, category=ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, sector_stage_codes = naics_foodsystem$stage_code)

# Actually run the optimization, printing diagnostic output at each iteration
# It looks like each iteration will take 1 or 2 minutes so the whole process could take a pretty long time.
res <- nloptr(x0 = x0, eval_f = eval_f_eeio, eval_grad_f = eval_grad_f_eeio, lb = lb, ub = ub, eval_g_ineq = NULL, eval_g_eq = eval_eq_totalcost, opts = opts, category = ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, sector_stage_codes = naics_foodsystem$stage_code, Ctotal = Ctotal)
res


# Run optimization with NlcOptim ------------------------------------------

tol <- 1e-6
res2 <- solnl(X = x0, objfun = eval_f_eeio, confun = eval_eq_totalcost_ceq, lb = lb, ub = ub, tolX = tol, tolFun = tol, tolCon = tol, maxIter = 1000, category = ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, sector_stage_codes = naics_foodsystem$stage_code, Ctotal = Ctotal)


# Run optimization with Rsolnp --------------------------------------------

# Use default control parameters

res3 <- solnp(pars = x0, fun = eval_f_eeio, eqfun = eval_eq_total, eqB = Ctotal, LB = lb, UB = ub, category = ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, sector_stage_codes = naics_foodsystem$stage_code, Ctotal = Ctotal)

# What are the final waste rates?
xfinal <- setNames(res3$pars, c('L1', 'L2', 'L3'))
optimal_waste_rate <- waste_rate_by_cost(x = xfinal[sector_stage_codes], W0 = baseline_waste_rate, Wu = fake_unavoidable_waste_rate, B = B_sectors, nu = nu_sectors)

# Graph all.
waste_rate_df <- data.frame(name = naics_foodsystem$BEA_389_def, stage = sector_stage_codes, baseline = baseline_waste_rate, unavoidable = fake_unavoidable_waste_rate, optimal = optimal_waste_rate) %>%
  gather(scenario, waste_rate, -name, -stage)

ggplot(waste_rate_df, aes())