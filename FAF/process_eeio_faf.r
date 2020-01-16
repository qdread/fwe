# Process FAF EEIO output

library(tidyverse)

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
  filter(BEA_Code %in% BEA_food$BEA_389_code)

all_codes <- read.csv(file.path(fp_crosswalk, 'all_codes.csv'), stringsAsFactors = FALSE)

# Create vectors for each pair of regions ---------------------------------

# Match codes with long code name
code_lookup <- all_codes %>%
  transmute(BEA_Code = sector_code_uppercase, BEA_code_full = sector_desc_drc)

# Get a unique vector for each combination (make into a list) -- add up by trade type and mode
faf_vectors <- faf_by_bea %>%
  left_join(code_lookup) %>%
  group_by(fr_orig, dms_orig, dms_dest, fr_dest, trade_type, BEA_code_full) %>%
  summarize(value = sum(value_2012) * 1e6) %>%
  ungroup %>%
  nest(data = c(BEA_code_full, value))

# Load impacts
load('/nfs/qread-data/cfs_io_analysis/faf_eeio_output.RData')

impacts <- bind_rows(map(impacts, ~ as.data.frame(t(.))))

faf_impacts <- bind_cols(faf_vectors, impacts)

save(faf_impacts, file = '/nfs/qread-data/cfs_io_analysis/faf_eeio_output_full.RData')

