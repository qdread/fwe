# Combine SUSB and NASS data by NAICS code to produce a better weighting (previous one only used QCEW = bad)
# QDR / FWE / 05 Dec 2019

# Modify code that was in sctg_to_bea.r

# Load data -----------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Z:/')

fp <- ifelse(is_local, 'Z:', '/nfs/fwe-data')
fpq <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'commodity_flows/CFS')
fp_faf <- file.path(fp, 'commodity_flows/FAF')
fp_satellite <- file.path(fp, 'IO_tables/output_csvs')
fp_crosswalk <- file.path(fpq, 'crossreference_tables')
fp_out <- file.path(fpq, 'cfs_io_analysis')

faf <- read_csv(file.path(fp_faf, 'FAF4.4.1.csv'), col_types = paste(c(rep('f', 9), rep('n', 31-9)), collapse = ''))

# Load crosswalk table
load(file.path(fp_crosswalk, 'NAICS_BEA_SCTG_crosswalk.RData'))

# Fips codes
fips <- read_delim('/nfs/qread-data/statefips.txt', delim = '|')

# FAF region lookup
faf_lookup <- read_csv(file.path(fp_faf, 'faf4_region_lookup.csv'))


# Read CDQT (NASS) data for the agricultural NAICS codes
nass_naics <- read_csv(file.path(fp_out, 'NASS2012_receipts_workers_NAICS.csv'))

# Read SUSB data for all other NAICS codes
susb12 <- read_csv(file.path(fp, 'Census/SUSB/us_state_6digitnaics_2012.txt'), col_types = 'fffnnnffnfnfccc') 
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
nass_naics %>% filter(FIPS %in% '99', grepl('^1119', NAICS)) # Yes, it is. Row 1 is equal to the sums of rows 2-4. 

# However we cannot ignore the less than 6 digit NAICS codes because most of it is actually less than 6 digits.
# Some of the four digit NAICS codes in the NASS data actually correspond to multiple BEA codes, so that's unfortunate.

# Check overlaps
intersect(nass_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(nass_naics_notredundant, bea_naics$related_2012_NAICS_6digit) # These are all 4 and 5 digit codes.
setdiff(bea_naics$related_2012_NAICS_6digit, nass_naics_notredundant)

# Number of BEA codes associated with each non redundant NAICS code in NASS
map_int(nass_naics_notredundant, ~ length(unique(bea_naics$BEA_Code[grepl(paste0('^', .), bea_naics$related_2012_NAICS_6digit)])))
# None are a problem except that oilseeds and grains are lumped into a single NASS NAICS code.

# We will use NASS to find ratios of oilseed and grain production within each state to disaggregate code 1111 into 1111A and 1111B.

# Combine SUSB and NASS data ----------------------------------------------

# Combine SUSB and NASS so that SUSB covers code 113*** and above, and NASS covers 111*** and 112***.


