# Correct CFS map to add codes and corrected names

library(tidyverse)
library(sf)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fp_cfs <- file.path(fp, 'commodity_flows/CFS')
fp_faf <- file.path(fp, 'commodity_flows/FAF')
fp_satellite <- file.path(fp, 'IO_tables/output_csvs')
fp_crosswalk <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')

load(file.path(fp_out, 'faf_by_bea.RData'))
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions'), layer = 'cfs_aea') %>% 
  mutate(FAF_Region = gsub('[ ]{2,}', ' ', as.character(CFS12_NAME)))
faf_lookup <- read_csv(file.path(fp_faf, 'faf4_region_lookup.csv')) %>%
  select(-(X6:X9)) %>%
  setNames(gsub(' ', '_', names(.))) %>%
  mutate(FAF_Region = gsub('\n', ' ', FAF_Region))

# Check matches
setdiff(cfsmap$FAF_Region, faf_lookup$FAF_Region)

# Fix remaining discrepancies
cfsmap$FAF_Region[cfsmap$FAF_Region %in% c('Remainder of Alaska', 'Remainder of Idaho')] <- c('Alaska', 'Idaho')

# Write corrected FAF lookup table and cfs map joined (write as geojson)
cfsmap <- cfsmap %>% left_join(faf_lookup)
st_write(cfsmap, dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions','cfs_aea.geojson'), driver = 'GeoJSON')

write_csv(faf_lookup, file.path(fp_out, 'faf_region_lookup.csv'))
