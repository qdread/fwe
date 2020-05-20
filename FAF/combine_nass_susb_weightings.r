# Combine SUSB and NASS data by NAICS code to produce a better weighting (previous one only used QCEW = bad)
# QDR / FWE / 05 Dec 2019

# Modified 19 May 2020: Use newer imputed NASS data that also includes cropland and pastureland
# Modified 09 Dec 2019: Use imputed values from NASS
# Originally modified from code that was in sctg_to_bea.r

# Load data -----------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_satellite <- file.path(fp, 'raw_data/IO_tables/output_csvs')
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_out <- file.path(fp, 'cfs_io_analysis')

# Load crosswalk table
load(file.path(fp_crosswalk, 'NAICS_BEA_SCTG_crosswalk.RData'))

# Fips codes
fips <- read_delim('/nfs/qread-data/statefips.txt', delim = '|')

# Read CDQT (NASS) data for the agricultural NAICS codes
nass_naics <- read_csv(file.path(fp_out, 'NASS2012_receipts_workers_land_NAICS_imputed.csv'))

# Read SUSB data for all other NAICS codes
susb12 <- read_csv(file.path(fp, 'raw_data/Census/SUSB/us_state_6digitnaics_2012.txt'), col_types = 'fffnnnffnfnfccc') 
susb_total <- susb12 %>% 
  filter(ENTRSIZEDSCR %in% 'Total')

# Read crosswalk that maps NAICS 07 and NAICS 12 to the BEA codes
bea_naics <- read_csv(file.path(fp_crosswalk, 'BEA_NAICS07_NAICS12_crosswalk.csv'))

# Summarize n of employees for SUSB ---------------------------------------

# For each SCTG code, find the numbers of employees in each of the NAICS codes that go with it in each state.
# Also use receipts.

# This must be a two step process. First go through the BEA codes and get the relative weightings by NAICS by state
# Then go back through and aggregate even further up to SCTG codes.

# 1. get rid of bea sectors with no sctg code 
concordance_sctgonly <- concordance %>%
  filter(!is.na(SCTG.Code))

# Total employees and receipts by NAICS code by state in SUSB - there is no need to group sum them because they already represent totals.
# However we have a lot of different levels of NAICS codes.
susb_total <- susb_total %>% 
  mutate(RCPT_N = RCPT_N * 1000) %>%
  select(STATE, STATEDSCR, NAICS, NAICSDSCR, EMPL_N, RCPT_N) %>%
  setNames(c('state_fips', 'state_name', 'NAICS', 'NAICS_description', 'employees', 'receipts'))


# Map SUSB to BEA (codes 113 and above) -----------------------------------

# Unique codes in the SUSB dataset
susb_naics <- unique(gsub('-','', susb_total$NAICS))

# First get rid of any redundant ones in the SUSB that have a longer and more specific code.
susb_redundant <- map_lgl(susb_naics, ~ nchar(.) < max(nchar(grep(paste0('^', .), susb_naics, value = TRUE))))
susb_naics_notredundant <- susb_naics[!susb_redundant] # Some of these are actually less than 6 characters.

# See if the <6 characters are in FSC.
shortcodes <- susb_naics_notredundant[nchar(susb_naics_notredundant) < 6]
susb_total %>% filter(NAICS %in% c('44-45','48-49')) # We can ignore these.

susb_naics_notredundant <- susb_naics_notredundant[nchar(susb_naics_notredundant) == 6] # This begins at 113110 so that's good.

# Check which codes are in the crosswalk and which aren't
intersect(susb_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(susb_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(bea_naics$related_2012_NAICS_6digit, susb_naics_notredundant) # The primary ag codes so that's good.
susb_total %>% filter(NAICS %in% c('517911','517919')) # Not relevant so OK.

# combine them so that any "more specific" one is matched to its less specific parent code.
susb_naics_matchidx <- map_int(susb_naics_notredundant, function(code) {
  subcodes <- map(2:nchar(code), ~ substr(code, 1, .)) # all possible subcodes
  match_idx <- map(subcodes, ~ grep(paste0('^', .), bea_naics$related_2012_NAICS_6digit))
  # Find the longest matching code
  longest_match <- max(which(map_int(match_idx, length) > 0))
  ifelse(longest_match > 0, match_idx[[longest_match]], NA)
})

# Get BEA codes corresponding to the matches
susb_bea_lookup <- data.frame(NAICS = susb_naics_notredundant, 
                              BEA_code = bea_naics$BEA_Code[susb_naics_matchidx])

# Remove redundant rows from the SUSB dataset and add column for BEA code
susb_bea <- susb_total %>%
  filter(NAICS %in% susb_naics_notredundant) %>%
  left_join(susb_bea_lookup) %>%
  select(-NAICS, -NAICS_description) %>%
  group_by(state_fips, state_name, BEA_code) %>%
  summarize_all(sum)


# Map NASS to BEA (codes 111 and 112) -------------------------------------

# We have a complication where 11193,11194,11199 are in a single classification, as well as 1125 and 1129.
# Check whether these are included in the same BEA codes. If so we can just lump them under one code.

bea_naics %>% filter(grepl('^1119', related_2012_NAICS_6digit)) # Only 1 code. "other crops" In fact all 1119 are included under this.
bea_naics %>% filter(grepl('^1125|^1129', related_2012_NAICS_6digit)) # Only 1 BEA code. It's all included under other animal production (all except cows and chickens)

# Take only the first string before the first space character in the NASS NAICS codes.
nass_naics <- nass_naics %>%
  mutate(NAICS = map_chr(strsplit(NAICS, split = ' '), 1))

nass_uniquenaics <- unique(nass_naics$NAICS)

# First get rid of any redundant ones in the NASS that have a longer and more specific code.
nass_redundant <- map_lgl(nass_uniquenaics, ~ nchar(.) < max(nchar(grep(paste0('^', .), nass_uniquenaics, value = TRUE))))
nass_naics_notredundant <- nass_uniquenaics[!nass_redundant] # Some of these are actually less than 6 characters.

# Check and make sure the 1119 is in fact redundant
nass_naics %>% filter(state_fips %in% '99', grepl('^1119', NAICS)) # Yes, it is. Row 1 is equal to the sums of rows 2-4. 

# However we cannot ignore the less than 6 digit NAICS codes because most of it is actually less than 6 digits.
# Some of the four digit NAICS codes in the NASS data actually correspond to multiple BEA codes, so that's unfortunate.

# Check overlaps
intersect(nass_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(nass_naics_notredundant, bea_naics$related_2012_NAICS_6digit) # These are all 4 and 5 digit codes.
setdiff(bea_naics$related_2012_NAICS_6digit, nass_naics_notredundant)

# Number of BEA codes associated with each non redundant NAICS code in NASS
map_int(nass_naics_notredundant, ~ length(unique(bea_naics$BEA_Code[grepl(paste0('^', .), bea_naics$related_2012_NAICS_6digit)])))
# None are a problem except that oilseeds and grains are lumped into a single NASS NAICS code.

# Use NASS to find ratios of oilseed and grain production within each state to disaggregate code 1111 into 1111A and 1111B.
# That is done in another script, disaggregate_oilseed_and_grain.r. Read in the result of that script and use to disaggregate.
oilseed_grain_proportions <- read_csv(file.path(fp_out, 'oilseed_grain_proportions.csv') )

# Create disaggregated grain and oilseed data.
nass1111 <- nass_naics %>% 
  filter(NAICS %in% '1111') %>%
  left_join(oilseed_grain_proportions) %>%
  select(-grain, -oilseed) %>%
  pivot_longer(cols = c(proportion_grain, proportion_oilseed), names_to = 'crop', values_to = 'proportion') %>%
  mutate(receipts = round(receipts * proportion),
         n_workers = round(n_workers * proportion),
         cropland = round(cropland * proportion),
         pastureland = round(pastureland * proportion))

nass1111_edited <- nass1111 %>%
  mutate(NAICS = if_else(crop == 'proportion_grain', '111130', '111110')) %>% # These are just one of the naics codes we could use.
  select(-crop, -proportion)

nass_naics_edited <- rbind(nass1111_edited, nass_naics %>% filter(!NAICS %in% '1111'))


# Transform nass naics to nass bea ----------------------------------------

# combine them so that any "more specific" one is matched to its less specific parent code.
nass_naics_notredundant_modified <- c('111110','111130', nass_naics_notredundant[-1])

nass_naics_matchidx <- map_int(nass_naics_notredundant_modified, function(code) {
  subcodes <- map(2:nchar(code), ~ substr(code, 1, .)) # all possible subcodes
  match_idx <- map(subcodes, ~ grep(paste0('^', .), bea_naics$related_2012_NAICS_6digit))
  # Find the longest matching code
  longest_match <- max(which(map_int(match_idx, length) > 0))
  ifelse(longest_match > 0, match_idx[[longest_match]], NA)
})

# Get BEA codes corresponding to the matches
nass_bea_lookup <- data.frame(NAICS = nass_naics_notredundant_modified, 
                              BEA_code = bea_naics$BEA_Code[nass_naics_matchidx])

# Remove redundant rows from the NASS dataset and add column for BEA code
nass_bea <- nass_naics_edited %>%
  filter(NAICS %in% nass_naics_notredundant_modified) %>%
  left_join(nass_bea_lookup) %>%
  select(-NAICS) %>%
  group_by(level, state_fips, state_abbrev, state_name, BEA_code) %>%
  summarize_all(sum)


# Combine SUSB and NASS data ----------------------------------------------

# Combine SUSB and NASS so that SUSB covers code 113*** and above, and NASS covers 111*** and 112***.

nass_bea_edited <- nass_bea %>%
  ungroup %>%
  select(state_fips, state_name, BEA_code, n_workers, receipts, cropland, pastureland) %>%
  setNames(c(names(susb_bea), 'cropland', 'pastureland'))

susb_nass_bea <- bind_rows(nass_bea_edited, ungroup(susb_bea)) %>%
  mutate(state_name = if_else(state_name == 'United States', 'US TOTAL', state_name),
         state_name = toupper(state_name))

write_csv(susb_nass_bea, file.path(fp_out, 'susb_nass_workers_receipts_land_bea.csv'))
