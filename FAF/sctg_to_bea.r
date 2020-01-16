# Convert SCTG codes of shipments among states to BEA codes
# Part of workflow: Bridge EEIO with FAF to get virtual transfers
# QDR / FWE / 04 Oct 2019

# Modified 07 Dec 2019: Redo the weighting with the improved combined susb-nass numbers (receipts not employees)

# Load FAF data -----------------------------------------------------------

library(tidyverse)

fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data/raw_data')
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

# Load the BEA mapping to SUSB and NASS codes (already created in another script, aggregated from NAICS codes!)

bea_lookup <- read_csv(file.path(fp_out, 'susb_nass_workers_receipts_bea.csv'))

# For each SCTG code, find the numbers of employees in each of the BEA codes that go with it in each state.

# Get rid of bea sectors with no sctg code 
concordance_sctgonly <- concordance %>%
  filter(!is.na(SCTG.Code))

# Check duplicated BEA codes in concordance
sort(table(concordance_sctgonly$BEA.Code.and.Title))
concordance_sctgonly %>% filter(BEA.Code.and.Title %in% c('212310','2123A0','324110')) # These are not relevant to us.


# Join BEA table with SCTG-NAICS-BEA concordance
# Remove the duplicated rows.
bea_sctg_lookup <- bea_lookup %>%
  left_join(concordance_sctgonly, by = c('BEA_code' = 'BEA.Code.and.Title'))

dup_idx <- which(duplicated(bea_sctg_lookup[,c('state_fips','state_name','BEA_code')]))

bea_sctg_lookup <- bea_sctg_lookup[-dup_idx, ]

# Widen the lookup
bea_sctg_data_wide <- bea_sctg_lookup %>%
  filter(!is.na(SCTG.Code)) %>%
  select(state_name, SCTG.Code, BEA_code, receipts) %>%
  rename(SCTG_Code = SCTG.Code) %>%
  mutate(SCTG_Code = sprintf('%02d', SCTG_Code)) %>%
  pivot_wider(names_from = state_name, values_from = receipts, values_fill = list(receipts = 0))

# Lookup for state codes
unique_fips <- unique(bea_sctg_lookup$state_fips)
unique_states <- paste('US', with(fips, STUSAB[match(unique_fips, STATE)]), sep = '_')


# Check which have no receipt totals for some codes
which_missing <- bea_sctg_data_wide %>%
  group_by(SCTG_Code) %>%
  summarize_if(is.numeric, ~ sum(.x) > 0)


# Write output ------------------------------------------------------------

names(bea_sctg_data_wide)[1:2] <- c('SCTG_Code', 'BEA_Code')
#write_csv(bea_sctg_data_wide, file.path(fp_out, 'receipts_bea_sctg_x_state.csv'))

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

# Reshape BEA receipts to long
bea_sctg_long <- bea_sctg_data_wide %>%
  pivot_longer(-c(SCTG_Code, BEA_Code), names_to = 'state_name')

# Widen across BEA codes
receipts_state_x_industry <- bea_sctg_long %>%
  pivot_wider(names_from = BEA_Code, values_fill = list(value = 0))

# Replace all rows with zero for every BEA code with the US total value 
US_total_receipts <- receipts_state_x_industry %>% filter(state_name %in% 'US TOTAL')

all_zero_rows <- apply(receipts_state_x_industry[,-(1:2)], 1, sum) == 0

for (i in 1:nrow(receipts_state_x_industry)) {
  if (all_zero_rows[i]) {
    receipts_state_x_industry[i, -(1:2)] <- US_total_receipts[which(US_total_receipts$SCTG_Code == receipts_state_x_industry$SCTG_Code[i]), -(1:2)]
  }
}

table(apply(receipts_state_x_industry[,-(1:2)], 1, sum) == 0) # all have at least one nonzero entry now.

# Make a column to note whether the weightings are nationwide values or specific to the state.
receipts_state_x_industry <- receipts_state_x_industry %>%
  mutate(weighting_source = if_else(all_zero_rows, 'US','state'))

# Now that the missing values have been replaced, reshape the df back to a long form.
receipts_long_fixed <- receipts_state_x_industry %>%
  pivot_longer(-c(SCTG_Code, state_name, weighting_source), names_to = 'BEA_Code', values_to = 'receipts') %>%
  filter(receipts > 0)

# Normalize to 1 within state and SCTG code
receipts_long_fixed <- receipts_long_fixed %>%
  group_by(SCTG_Code, state_name) %>%
  mutate(receipts_weight = receipts / sum(receipts)) %>%
  ungroup

# Join with the correct state abbreviation.
fips_fixed <- fips %>% 
  select(-STATENS) %>% 
  setNames(c('state_fips', 'state_abbrev', 'state_name')) %>% 
  mutate(state_name = toupper(state_name)) %>% 
  add_row(state_fips = '99', state_abbrev = 'US', state_name = 'US TOTAL')

receipts_long_fixed <- receipts_long_fixed %>% 
  left_join(fips_fixed) 

# Join FAF and employee weights 
faf_join_weights <- faf %>%
  rename(state_abbrev = State, SCTG_Code = sctg2) %>%
  left_join(receipts_long_fixed) %>%
  mutate_at(c('tons_2012', 'value_2012', 'tmiles_2012', 'wgt_dist'), ~ .x * receipts_weight)

# To save space, create a smaller tibble with only the relevant columns that can be joined back with the other info later, database style
# This result still has 13 million rows and 15 columns :-O
# The table to join it with is the faf_region_lookup table, to be joined by dms_orig column.
# We can get rid of the employee numbers and weights columns because that info is in other datasets
# Convert weighting source to 1 for state, 2 for US
faf_by_bea <- faf_join_weights %>%
  select(fr_orig:trade_type, BEA_Code, tons_2012:wgt_dist, weighting_source) %>%
  mutate(weighting_source = if_else(weighting_source == 'state', 1, 2))

# Create a smaller summary table with all modes summed up so that we can make maps
faf_by_bea_allmodes <- faf_by_bea %>%
  group_by(fr_orig, dms_orig, dms_dest, fr_dest, BEA_Code) %>%
  summarize(tons_2012 = sum(tons_2012), value_2012 = sum(value_2012), tmiles_2012 = sum(tmiles_2012))

# Write output ------------------------------------------------------------

# Write final, correct version of the number of employees by state so that we have that for later
write_csv(receipts_long_fixed, file.path(fp_out, 'receipts_bea_x_state_long_final.csv'))

# Write FAF by BEA as .RData so we have that for later and to save space compared to CSV
save(faf_by_bea, file = file.path(fp_out, 'faf_by_bea.RData'))
save(faf_by_bea_allmodes, file = file.path(fp_out, 'faf_by_bea_allmodes.RData'))
