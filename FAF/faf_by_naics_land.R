# Agricultural land by NAICS by state, converted to FAF region
# QDR / FWE / 19 May 2020

# Also do FAF by BEA

# Load data ---------------------------------------------------------------


library(tidyverse)
library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_out <- ifelse(is_local, 'Q:/cfs_io_analysis', '/nfs/qread-data/cfs_io_analysis')

nass_data <- read_csv(file.path(fp_out, 'NASS2012_receipts_workers_land_NAICS_imputed.csv'))
susb_nass_bea <- read_csv(file.path(fp_out, 'susb_nass_workers_receipts_land_bea.csv'))
county_land <- read_csv(file.path(fp_out, 'cropland_by_county_FAF_joined.csv'))


# Assign land to FAF proportionally ---------------------------------------

# We have land down to NAICS code, but only at the state level
# We have total crop and pastureland at the county level, and counties assigned to FAF
# For each state x faf region combo, get the total amount of crop and pastureland.
# Then, use that factor to apportion the state level NAICS land totals to FAFs.

state_x_faf_land <- county_land %>%
  group_by(state_code, state_abbrev, FAF_Region) %>%
  summarize(cropland = sum(cropland, na.rm = TRUE), pastureland = sum(pastureland, na.rm = TRUE))

fafmap <- st_read(file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))
setdiff(fafmap$FAF_Region, state_x_faf_land$FAF_Region) # Washington, DC isn't included. Just set its cropland and pastureland to zero.

state_x_faf_land <- state_x_faf_land %>%
  ungroup %>%
  add_row(state_code = '11', state_abbrev = 'DC', FAF_Region = "Washington-Arlington-Alexandria, DC-VA-MD-WV CFS Area (DC Part)", cropland = 0, pastureland = 0)

state_x_faf_proportions <- state_x_faf_land %>%
  group_by(state_code, state_abbrev) %>%
  mutate(cropland_proportion = cropland/sum(cropland), pastureland_proportion = pastureland/sum(pastureland)) %>%
  select(-cropland, -pastureland)

state_lookup <- unique(state_x_faf_land[,1:2])

nass_land_normalized <- nass_data %>%
  filter(!state_abbrev %in% 'US') %>%
  select(state_abbrev, NAICS, cropland, pastureland) %>%
  full_join(state_x_faf_proportions) %>% 
  mutate(cropland_normalized = cropland * cropland_proportion, pastureland_normalized = pastureland * pastureland_proportion)

nass_bea_land_normalized <- susb_nass_bea %>%
  filter(!is.na(cropland), !is.na(pastureland), !state_fips %in% '99') %>%
  rename(state_code = state_fips) %>%
  left_join(state_lookup) %>%
  select(state_abbrev, BEA_code, cropland, pastureland) %>%
  full_join(state_x_faf_proportions) %>% 
  mutate(cropland_normalized = cropland * cropland_proportion, pastureland_normalized = pastureland * pastureland_proportion)
  

# Create matrices of faf vs naics -----------------------------------------

cropland_mat <- nass_land_normalized %>%
  select(NAICS, FAF_Region, cropland_normalized) %>%
  pivot_wider(id_cols = FAF_Region, names_from = NAICS, values_from = cropland_normalized, values_fill = list(cropland_normalized = 0)) %>%
  select(-`NA`)

pastureland_mat <- nass_land_normalized %>%
  select(NAICS, FAF_Region, pastureland_normalized) %>%
  pivot_wider(id_cols = FAF_Region, names_from = NAICS, values_from = pastureland_normalized, values_fill = list(pastureland_normalized = 0)) %>%
  select(-`NA`)


# Create matrices of faf vs bea -------------------------------------------

cropland_mat_bea <- nass_bea_land_normalized %>%
  select(BEA_code, FAF_Region, cropland_normalized) %>%
  pivot_wider(id_cols = FAF_Region, names_from = BEA_code, values_from = cropland_normalized, values_fill = list(cropland_normalized = 0)) %>%
  select(-`NA`)

pastureland_mat_bea <- nass_bea_land_normalized %>%
  select(BEA_code, FAF_Region, pastureland_normalized) %>%
  pivot_wider(id_cols = FAF_Region, names_from = BEA_code, values_from = pastureland_normalized, values_fill = list(pastureland_normalized = 0)) %>%
  select(-`NA`)

# Write outputs -----------------------------------------------------------

write_csv(cropland_mat, file.path(fp_out, 'faf_by_naics_cropland.csv'))
write_csv(pastureland_mat, file.path(fp_out, 'faf_by_naics_pastureland.csv'))
write_csv(nass_land_normalized, file.path(fp_out, 'nass_state_x_faf_land_totals.csv'))

write_csv(cropland_mat_bea, file.path(fp_out, 'faf_by_bea_cropland.csv'))
write_csv(pastureland_mat_bea, file.path(fp_out, 'faf_by_bea_pastureland.csv'))
write_csv(nass_bea_land_normalized, file.path(fp_out, 'nass_bea_state_x_faf_land_totals.csv'))
