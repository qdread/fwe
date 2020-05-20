# Get cropland and pastureland by FAF region

library(tidyverse)
library(sf)


is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_out <- ifelse(is_local, 'Q:/cfs_io_analysis', '/nfs/qread-data/cfs_io_analysis')


county_land <- read_csv(file.path(fp_out, 'cropland_by_county.csv'))

fafmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions'), layer = 'faf_aea')

# Join counties

county_land <- county_land %>% mutate(state_county_code = paste0(state_code, county_code))
county_land <- county_land %>%
  left_join(fafmap %>% as_tibble %>% select(ANSI_ST_CO, CNTY_NAME, CFS12_NAME), by = c('state_county_code' = 'ANSI_ST_CO'))

# Fill in the missing values
county_land %>% filter(state_abbrev %in% c('AK','SD'))

county_land <- county_land %>%
  mutate(FAF_Region = gsub('[ ]{2,}', ' ', as.character(CFS12_NAME))) %>%
  mutate(FAF_Region = case_when(
    is.na(FAF_Region) & state_abbrev == 'AK' ~ 'Remainder of Alaska',
    is.na(FAF_Region) & state_abbrev == 'SD' ~ 'Remainder of South Dakota',
    TRUE ~ FAF_Region
  )) 

# Correct two other wrong values
county_land <- county_land %>%
  mutate(FAF_Region = case_when(
    FAF_Region == 'Remainder of Alaska' ~ 'Alaska',
    FAF_Region == 'Remainder of Idaho' ~ 'Idaho',
    TRUE ~ FAF_Region
  ))

write_csv(county_land, file.path(fp_out, 'cropland_by_county_FAF_joined.csv'))
