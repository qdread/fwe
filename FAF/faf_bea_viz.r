# Create some visualizations and/or maps of the FAF data coded as BEA
# QDR / FWE / 07 Oct 2019


# Load data ---------------------------------------------------------------

library(tidyverse)
library(sf)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fp_cfs <- file.path(fp, 'commodity_flows/CFS')
fp_faf <- file.path(fp, 'commodity_flows/FAF')
fp_satellite <- file.path(fp, 'IO_tables/output_csvs')
fp_crosswalk <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')

load(file.path(fp_out, 'faf_by_bea.RData'))
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))
faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))


# Summarize faf data for map ----------------------------------------------

# Domestic outbound (wide)
faf_outbound <- faf_by_bea %>%
  filter(trade_type == 1) %>%
  group_by(dms_orig, BEA_code) %>%
  summarize(value = sum(value_2012)) %>%
  spread(BEA_code, value)

cfsmap_outbound <- cfsmap %>% left_join(faf_outbound, by = c('Code' = 'dms_orig'))

faf_inbound <- faf_by_bea %>%
  filter(trade_type == 1) %>%
  group_by(dms_dest, BEA_code) %>%
  summarize(value = sum(value_2012)) %>%
  spread(BEA_code, value)

cfsmap_inbound <- cfsmap %>% left_join(faf_inbound, by = c('Code' = 'dms_dest'))


# Make some maps ----------------------------------------------------------

# If we want to exclude AK, HI
ak_hi_idx <- grepl('Alaska|Hawaii|Honolulu', cfsmap$FAF_Region)

plot(cfsmap_outbound[c('1111A0', '1111B0', '111200', '111300')])

ggplot(cfsmap_outbound %>% filter(!ak_hi_idx)) +
  geom_sf(aes(fill = `1111A0`))
