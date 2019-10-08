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
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.geojson'))
faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))


# Summarize faf data for map ----------------------------------------------


