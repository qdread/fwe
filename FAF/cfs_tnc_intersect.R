# Conversion of FAF regions to ecoregions, using cropland map to determine how much land is in each ecoregion portion of each FAF region.
# Tabulate 2012 CDL by combination of FAF polygon and TNC ecoregion
# So first we need to create a combined polygon layer that contains the intersection of every FAF polygon and TNC ecoregion.

library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_fwe <- ifelse(is_local, '~/Documents/GitHub/fwe', '~/fwe')

# Load CFS(FAF) and TNC polygons
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))
tncmap <- st_read(dsn = file.path(fp, 'raw_data/landuse/ecoregions'), layer = 'tnc_usa_aea')

# Intersect the two
cfs_tnc <- st_intersection(cfsmap, tncmap) 
# This results in 453 polygons combining the two
plot(cfs_tnc['Code']) # Looks correct

# Write result as geopackage
st_write(cfs_tnc, dsn = file.path(fp_out, 'cfs_tnc_aea_intersect.gpkg'))

# Calculate areas of all intersected polygons
cfs_tnc_area <- st_area(cfs_tnc)

# Extract attributes from the geometry, and include area column, to make a long and wide form of the area table
cfs_tnc_area_df <- cfs_tnc %>%
  as_tibble %>%
  select(FAF_Region, Code, State, ECO_ID_U, ECO_CODE, region) %>%
  mutate(area = cfs_tnc_area)

# Create a table by FAF code 

cfs_tnc_area_mat <- cfs_tnc_area_df %>%
  select(Code, ECO_CODE, area) %>%
  mutate(area = as.numeric(area)) %>%
  pivot_wider(names_from = ECO_CODE, values_from = area, values_fill = list(area = 0))

# The tabulation of the CDL raster by this intersected polygon layer is done in Python from a shell script
