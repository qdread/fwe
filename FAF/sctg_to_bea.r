# Convert SCTG codes of shipments among states to BEA codes
# Part of workflow: Bridge EEIO with FAF to get virtual transfers
# QDR / FWE / 04 Oct 2019


# Load FAF data -----------------------------------------------------------

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fp_cfs <- file.path(fp, 'commodity_flows/CFS')
fp_faf <- file.path(fp, 'commodity_flows/FAF')
fp_satellite <- file.path(fp, 'IO_tables/output_csvs')
fp_crosswalk <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')

faf <- read_csv(file.path(fp_faf, 'FAF4.4.1.csv'), col_types = paste(c(rep('f', 9), rep('n', 31-9)), collapse = ''))

# Load crosswalk table
load(file.path(fp_crosswalk, 'NAICS_BEA_SCTG_crosswalk.RData'))

# Fips codes
fips <- read_delim('/nfs/qread-data/statefips.txt', delim = '|')

# FAF region lookup
faf_lookup <- read_csv(file.path(fp_faf, 'faf4_region_lookup.csv'))

# List BEA codes for all SCTG codes ---------------------------------------

SCTG_BEA_list <- concordance %>% 
  group_by(SCTG.Code) %>%
  group_modify( ~ tibble(BEA_codes = list(.x$BEA.Code.and.Title), NAICS_codes = list(do.call(c, .x$related_2007_NAICS_codes))))


# Get weightings for mapping ----------------------------------------------

# This is going to be a one to many mapping since there are ~40 SCTG codes but ~400 BEA codes.
# What we need to do is find the relative proportions of output from the origin state and assign them that way - SUSB and QCEW should be the ones for that.

susb12 <- read_csv(file.path(fp, 'Census/SUSB/us_state_6digitnaics_2012.txt'), col_types = 'fffnnnffnfnfccc') 
susb_total <- susb12 %>% 
  filter(ENTRSIZEDSCR %in% 'Total')
qcew12 <- read_csv(file.path(fp, 'Census/QCEW/2012.annual.singlefile.csv'))

# See https://data.bls.gov/cew/doc/titles/agglevel/agglevel_titles.htm for details on how this is split up
qcew_state_naics <- qcew12 %>%
  filter(agglvl_code %in% 54:58) %>%
  mutate(state_fips = substr(area_fips, 1, 2)) 

# Also do the US wide one for the cases where we don't have state data
qcew_uswide_naics <- qcew12 %>%
  filter(agglvl_code %in% 14:18)
  
  
# For each SCTG code, find the numbers of employees in each of the NAICS codes that go with it in each state.

# This must be a two step process. First go through the BEA codes and get the relative weightings by NAICS by state
# Then go back through and aggregate even further up to SCTG codes.

# 1. get rid of bea sectors with no sctg code 
concordance_sctgonly <- concordance %>%
  filter(!is.na(SCTG.Code))

# Total employees by NAICS code by state
industry_employees <- qcew_state_naics %>% 
  group_by(state_fips, industry_code) %>% 
  summarize(employees = sum(annual_avg_emplvl))

# Define function to get the employees for the NAICS codes associated with a single BEA code for each state
get_employee_totals <- function(naics_codes) {
  totals_list <- industry_employees %>%
    group_by(state_fips) %>%
    group_map(~ .x$employees[match(naics_codes, .x$industry_code)])
  totals_matrix <- do.call(cbind, totals_list) 
  rownames(totals_matrix) <- naics_codes
  totals_matrix[is.na(totals_matrix)] <- 0
  return(totals_matrix)
}

# Map the function to each set of NAICS codes corresponding to each BEA code
employees_industry_x_state <- map(concordance_sctgonly$related_2007_NAICS_codes, get_employee_totals)

# Lookup for state codes
unique_fips <- unique(industry_employees$state_fips)
unique_states <- paste('US', with(fips, STUSAB[match(unique_fips, STATE)]), sep = '_')

# Since I could not figure it out in a tidy way, join all the codes back together with the results
employees_industry_x_state <- do.call(rbind, employees_industry_x_state)
colnames(employees_industry_x_state) <- unique_states

employees_industry_x_state <- cbind(concordance_sctgonly[rep(1:nrow(concordance_sctgonly), map_int(concordance_sctgonly$related_2007_NAICS_codes, length)), 1:3], NAICS_Code = rownames(employees_industry_x_state), employees_industry_x_state)

employees_BEA_x_state <- employees_industry_x_state %>%
  group_by(SCTG.Code, BEA.Code.and.Title) %>%
  summarize_at(vars(starts_with('US')), sum)

# Check which have no employees for some codes
which_missing <- employees_BEA_x_state %>%
  group_by(SCTG.Code) %>%
  summarize_at(vars(starts_with('US')), ~ sum(.x) > 0)


# Write output ------------------------------------------------------------

# Just write the by industry one because it has more detail
names(employees_industry_x_state)[1:2] <- c('SCTG_Code', 'BEA_Code')
#write_csv(employees_industry_x_state, file.path(fp_out, 'employees_bea_x_state.csv'))


# Split SCTG values into BEA ----------------------------------------------

# FAF has data on transportation modes that we maybe don't care about? But probably a good idea to keep them distinct.
# For now keep all separate, but consider only monetary value in 2012. (keep tons also)
# That way we can look at the carbon footprint of miles traveled by different means.
# Also, get rid of code 99 which is unknown.
faf <- faf %>% 
  filter(!sctg2 %in% '99') %>%
  select(fr_orig:tons_2012, value_2012, tmiles_2012, wgt_dist) %>%
  left_join(faf_lookup %>% rename(FAF_Region = `FAF Region`) %>% select(Code, FAF_Region, State), by = c('dms_orig'='Code'))

#employees_industry_x_state <- read_csv(file.path(fp_out, 'employees_bea_x_state.csv'))

# Sum up employees by state and BEA code
employees_industry_x_state_sums <- employees_industry_x_state %>%
  mutate(SCTG_Code = sprintf('%02d', SCTG_Code)) %>%
  group_by(SCTG_Code, BEA_Code) %>%
  summarize_at(vars(starts_with('US')), sum) %>%
  ungroup %>%
  mutate(US_total = apply(.[,grep('US', names(.))],1,sum))
 
# Reshape to long
employees_long <- employees_industry_x_state_sums %>%
  gather(state, n_employees, -SCTG_Code, -BEA_Code)

# Widen across BEA codes
employees_state_x_industry <- employees_long %>%
  spread(BEA_Code, n_employees, fill = 0)

# Replace all rows with zero for every BEA code with the US total value 
US_total_emps <- employees_state_x_industry %>% filter(state %in% 'US_total')

all_zero_rows <- apply(employees_state_x_industry[,-(1:2)], 1, sum) == 0

for (i in 1:nrow(employees_state_x_industry)) {
  if (all_zero_rows[i]) {
    employees_state_x_industry[i, -(1:2)] <- US_total_emps[which(US_total_emps$SCTG_Code == employees_state_x_industry$SCTG_Code[i]), -(1:2)]
  }
}

table(apply(employees_state_x_industry[,-(1:2)], 1, sum) == 0) # all have at least one nonzero entry now.

# Make a column to note whether the weightings are nationwide values or specific to the state.
employees_state_x_industry <- employees_state_x_industry %>%
  mutate(weighting_source = if_else(all_zero_rows, 'US','state'))

# Now that the missing values have been replaced, reshape the df back to a long form.
employees_long_fixed <- employees_state_x_industry %>%
  gather(BEA_code, n_employees, -SCTG_Code, -state, -weighting_source) %>%
  filter(n_employees > 0)

# Normalize to 1 within state and SCTG code
employees_long_fixed <- employees_long_fixed %>%
  group_by(SCTG_Code, state) %>%
  mutate(empl_weight = n_employees / sum(n_employees)) %>%
  ungroup

# Join FAF and employee weights 
faf_join_empl <- faf %>%
  mutate(State = paste('US', State, sep = '_')) %>%
  rename(state = State, SCTG_Code = sctg2) %>%
  left_join(employees_long_fixed) %>%
  mutate_at(c('tons_2012', 'value_2012', 'tmiles_2012', 'wgt_dist'), ~ .x * empl_weight)

# To save space, create a smaller tibble with only the relevant columns that can be joined back with the other info later, database style
# This result still has 13 million rows and 15 columns :-O
# The table to join it with is the faf_region_lookup table, to be joined by dms_orig column.
# We can get rid of the employee numbers and weights columns because that info is in other datasets
# Convert weighting source to 1 for state, 2 for US
faf_by_bea <- faf_join_empl %>%
  select(fr_orig:trade_type, BEA_code, tons_2012:wgt_dist, weighting_source) %>%
  mutate(weighting_source = if_else(weighting_source == 'state', 1, 2))

# Create a smaller summary table with all modes summed up so that we can make maps
faf_by_bea_allmodes <- faf_by_bea %>%
  group_by(fr_orig, dms_orig, dms_dest, fr_dest, BEA_code) %>%
  summarize(tons_2012 = sum(tons_2012), value_2012 = sum(value_2012), tmiles_2012 = sum(tmiles_2012))

# Write output ------------------------------------------------------------

# Write final, correct version of the number of employees by state so that we have that for later
write_csv(employees_long_fixed, file.path(fp_out, 'employees_bea_x_state_long_final.csv'))

# Write FAF by BEA as .RData so we have that for later and to save space compared to CSV
save(faf_by_bea, file = file.path(fp_out, 'faf_by_bea.RData'))
save(faf_by_bea_allmodes, file = file.path(fp_out, 'faf_by_bea_allmodes.RData'))
