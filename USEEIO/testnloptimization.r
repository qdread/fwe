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
  # If nu = 1, this becomes the exponential curve
  A <- (W0 - (2 ^ (-1/nu)) * Wu) / (1 - 2 ^ (-1/nu))
  pmin(W0, A + (Wu - A)/((1 + exp(-B*x))^(1/nu)))
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

# Parameters by stage
# Assume faster returns on investment for level 1 and level 2
# Assume higher startup costs for level 1, intermediate for level 2, none for level 3
B_stages <- c(L1 = 0.005, L2 = 0.005, L3 = 0.002)
nu_stages <- c(L1 = 0.1, L2 = 0.2, L3 = 1)

B_sectors <- B_stages[naics_foodsystem$stage_code]
nu_sectors <- nu_stages[naics_foodsystem$stage_code]

# Find the proportion of waste reduction dollars allocated to each sector within each stage of the food supply chain
# Use the baseline values for gross output from each sector to get the weights. (this is T008 in the make table)

gross_outputs <- M[naics_foodsystem$BEA_389_code, 'T008']
gross_outputs_by_stage <- tapply(gross_outputs, naics_foodsystem$stage_code, sum)
proportion_gross_outputs <- gross_outputs / gross_outputs_by_stage[naics_foodsystem$stage_code]

# Constraint: total cost - pulled out of thin air.
Ctotal <- 1000

# Plot the different curves. (added 13 May)
# Generate the data.
params_for_curves <- data.frame(W0 = baseline_waste_rate, Wu = fake_unavoidable_waste_rate, B = B_sectors, nu = nu_sectors)
yvals_curves <- pmap(params_for_curves, waste_rate_by_cost, x = seq(0, 1000, 1))
dat_for_curves <- imap_dfr(yvals_curves, ~ data.frame(sector_name = naics_foodsystem$BEA_389_def[.y], stage = naics_foodsystem$stage_code[.y], params_for_curves[.y, ], x = seq(0, 1000, 1), y = .x))

cs <- RColorBrewer::brewer.pal(3, 'Set3')

ggplot(dat_for_curves, aes(x = x, y = y, color = stage, group = sector_name)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(type='qual', palette='Set2', labels = c('production', 'retail', 'consumption')) +
  scale_x_continuous(name = 'Cost (relative units)', expand = c(0,0)) +
  scale_y_continuous(name = 'Waste rate', expand = c(0,0), labels = scales::percent, limits = c(0, 0.26)) +
  ggtitle('\"Fake\" cost curves for each FSC sector', 'Each stage has multiple sectors')
ggsave('/nfs/qread-data/figures/costcurve_fake_example.png', height = 6, width = 6, dpi = 300)

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

# Do with several total cost amounts
Ctotal_vec <- c(500, 1000, 2000, 5000)
optim_bytotals <- map(Ctotal_vec, ~ solnp(pars = rep(.x/3, 3), fun = eval_f_eeio, eqfun = eval_eq_total, eqB = .x, LB = lb, UB = ub, category = ghg_name, W0_sectors = baseline_waste_rate, Wu_sectors = fake_unavoidable_waste_rate, B_sectors = B_sectors, nu_sectors = nu_sectors, p_sectors = proportion_gross_outputs, sector_stage_codes = naics_foodsystem$stage_code, Ctotal = .x))

# What are the final waste rates?
xfinal <- map(optim_bytotals, ~ setNames(.x$pars, c('L1', 'L2', 'L3')))
optimal_waste_rate <- map(xfinal, ~ waste_rate_by_cost(x = .x[naics_foodsystem$stage_code], W0 = baseline_waste_rate, Wu = fake_unavoidable_waste_rate, B = B_sectors, nu = nu_sectors))
optimal_waste_rate <- map2_dfr(optimal_waste_rate, Ctotal_vec, ~ data.frame(waste_rate = .x, total_cost = .y))

# Graph all.
waste_rate_df <- data.frame(name = naics_foodsystem$BEA_389_def, stage = naics_foodsystem$stage_code, waste_rate = c(baseline_waste_rate, optimal_waste_rate$waste_rate), total_cost = c(rep(0, nrow(naics_foodsystem)), optimal_waste_rate$total_cost)) 

# waste_rate_df <- data.frame(name = naics_foodsystem$BEA_389_def, stage = naics_foodsystem$stage_code, baseline = baseline_waste_rate, unavoidable = fake_unavoidable_waste_rate, optimal = optimal_waste_rate) %>%
#   gather(scenario, waste_rate, -name, -stage)

# jitter horizontally, showing baseline and optimal
# waste_rate_df %>%
#   filter(scenario %in% c('baseline', 'optimal')) %>%
#   mutate(scen_jitter = jitter(as.numeric(factor(scenario)), factor = 0.1)) %>%
# ggplot(aes(x = scen_jitter, y = waste_rate, color = stage, group = name)) +
#   scale_x_continuous(breaks = 1:2, labels = factor(c('baseline', 'optimal'))) +
#   scale_y_continuous(name = 'Waste rate', labels = scales::percent) +
#   scale_color_discrete(labels = c('production', 'retail', 'consumption')) +
#   geom_line(color = 'black') + geom_point() +
#   ggtitle('Waste rates minimizing GHG emissions given total cost') +
#   theme_bw() +
#   theme(axis.title.x = element_blank(), 
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         legend.position = 'bottom')
  
# Plot waste rates
set.seed(111)
waste_rate_df %>%
  mutate(cost_jitter = jitter(total_cost, factor = 0.5)) %>%
  ggplot(aes(x = cost_jitter, y = waste_rate, color = stage, group = name)) +
  scale_x_continuous(name = 'Total invested in FLW reduction', breaks = c(0, 500, 1000, 2000, 5000)) +
  scale_y_continuous(name = 'Waste rate', labels = scales::percent, expand = c(0,0), limits = c(0, 0.27)) +
  scale_color_brewer(type='qual', palette='Set2', labels = c('production', 'retail', 'consumption')) +
  geom_line(color = 'black') + geom_point() +
  ggtitle('Waste rates minimizing GHG emissions given total cost') +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom')
ggsave('/nfs/qread-data/figures/costcurve_fake_example_ratesbycost.png', height = 6, width = 6, dpi = 300)

# Plot optimal solutions
optimal_df <- map2_dfr(optim_bytotals, Ctotal_vec, ~ data.frame(total_cost = .y, stage = factor(c('production', 'retail', 'consumption'), levels = c('production', 'retail', 'consumption')), cost = .x$pars))

ggplot(optimal_df, aes(x = total_cost, y = cost, color = stage, group = stage)) +
  geom_point(size = 2) + geom_line() +
  scale_x_continuous(name = 'Total invested in FLW reduction', breaks = c(0, 500, 1000, 2000, 5000)) +
  scale_y_continuous(name = 'Amount invested in each stage') +
  scale_color_brewer(type='qual', palette='Set2') +
  ggtitle('Optimal allocation of FLW reduction funds to minimize GHG emissions', 'for a number of possible total investments') +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
       panel.grid.major.x = element_blank(), 
       panel.grid.minor.x = element_blank(),
       legend.position = 'bottom')
ggsave('/nfs/qread-data/figures/costcurve_fake_example_allocationsbycost.png', height = 6, width = 6, dpi = 300)
