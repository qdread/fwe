# Some code to test the new routines to modify and retotal the make and use tables.

source('USEEIO/load_scenario_data.r')

# Let's assume that we are reducing waste by 50%
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

demand_reductions <- demand_change_fn(W0 = baseline_waste_rate, r = 0.5, p = naics_foodsystem$proportion_food)


# First example: only change intermediate (scenarios 1,2,3)
# v <- vector of demand factors
# c_mod <- vector of columns to change, same length as v

MU_modified_intermediate <- modify_make_and_use(M, U, R = v, c_int_mod = c_mod, c_final_mod = character(0), r_mod = character(0))
MU_modified <- retotal_make_and_use(M = MU_modified_intermediate$M, U = MU_modified_intermediate$U)

# Scenario 1
v <- demand_reductions[naics_foodsystem$stage_code == 'L1']
c_mod <- naics_foodsystem$BEA_389_code[naics_foodsystem$stage_code == 'L1']
MU_modified_intermediate <- modify_make_and_use(M, U, R = v, c_int_mod = c_mod, c_final_mod = character(0), r_mod = character(0))
MU_modified <- retotal_make_and_use(M = MU_modified_intermediate$M, U = MU_modified_intermediate$U)

# Second example: only change final (scenario 6)
# r_mod <- vector of rows to change, same length as v

MU_modified_intermediate <- modify_make_and_use(M, U, R = v, c_int_mod = character(0), c_final_mod = 'F01000', r_mod = r_mod)
MU_modified <- retotal_make_and_use(M = MU_modified_intermediate$M, U = MU_modified_intermediate$U)

# Third example: change both intermediate and final (scenarios 4,5)
# c_mod and r_mod will be the same, I think.

MU_modified_intermediate <- modify_make_and_use(M, U, R = v, c_int_mod = c_mod, c_final_mod = 'F01000', r_mod = r_mod)
MU_modified <- retotal_make_and_use(M = MU_modified_intermediate$M, U = MU_modified_intermediate$U)

