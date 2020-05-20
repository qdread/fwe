# Virtual transfers of materials embodied in food, and in wasted food
# QDR / FWE / 19 Nov 2019

# Modified 18 May 2020: edit this so that EEIO is removed from loop

# Load data ---------------------------------------------------------------

library(tidyverse)
library(reticulate)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_satellite <- file.path(fp, 'IO_tables/output_csvs')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_fwe <- ifelse(is_local, '~/Documents/GitHub/fwe', '~/fwe')

# Load FAF data
load(file.path(fp_out, 'faf_by_bea.RData'))
faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))

# Load lookup table for BEA codes
load(file.path(fp_crosswalk, 'NAICS_BEA_SCTG_crosswalk.RData'))
concordance <- concordance %>% arrange(BEA.Code.and.Title)

# Load food system lookup table
BEA_food <- read.csv(file.path(fp_crosswalk, 'naics_crosswalk_final.csv'), stringsAsFactors = FALSE) %>% 
  filter(food_system %in% c('partial', 'y'))

# Keep only rows of faf_by_bea that are food system
faf_by_bea <- faf_by_bea %>%
  rename(BEA_code = BEA_Code) %>%
  filter(BEA_code %in% BEA_food$BEA_389_code)

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_fwe, 'USEEIO/eeio_lcia.py'))

all_codes <- read.csv(file.path(fp_crosswalk, 'all_codes.csv'), stringsAsFactors = FALSE)

# Create vectors for each pair of regions ---------------------------------

# For each pair of regions, get the total (regardless of mode) value of each BEA code going from a-->b and b-->a

# See how many combinations there are:
#faf_by_bea %>% select(fr_orig, dms_orig, dms_dest, fr_dest) %>% unique %>% nrow # approx 40K

# Match codes with long code name
code_lookup <- all_codes %>%
  transmute(BEA_code = sector_code_uppercase, BEA_code_full = sector_desc_drc)

# Get a unique vector for each combination (make into a list) -- add up by trade type and mode
faf_vectors <- faf_by_bea %>%
  left_join(code_lookup) %>%
  group_by(fr_orig, dms_orig, dms_dest, fr_dest, trade_type, BEA_code_full) %>%
  summarize(value = sum(value_2012) * 1e6) %>%
  ungroup %>%
  nest(data = c(BEA_code_full, value))


# Run USEEIO for each code in vectors -------------------------------------

BEA_food <- left_join(BEA_food, code_lookup, by = c('BEA_389_code' = 'BEA_code'))
EEIO_BEA <- map(BEA_food$BEA_code_full, ~ eeio_lcia('USEEIO2012', list(1), list(.))) # 76 codes
# convert to 21x76 matrix
EEIO_BEA_mat <- do.call(cbind, map(EEIO_BEA, 'Total'))
  

# Get USEEIO results for each vector ---------------------------------

get_eeio_result <- function(dat) {
  idx <- match(dat$BEA_code_full, BEA_food$BEA_code_full)
  data.frame(Total = EEIO_BEA_mat[, idx, drop = FALSE] %*% dat$value, row.names = row.names(EEIO_BEA[[1]]))
}

impacts <- map(faf_vectors$data, get_eeio_result)
  
save(impacts, file ='/nfs/qread-data/cfs_io_analysis/faf_eeio_output.RData')


