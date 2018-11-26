# Connect Hist LU trends with rolling BBS average

# hist_lu_byregion_long and bbs_fg_rolling_wide are the two to consider.

# First, we need to locate which of the US EPA L3 ecoregions each BBS route is in.
bbs_rtemeta <- read.csv('Q:/BBS/routes.csv')
bbs_rtemeta <- bbs_rtemeta %>%
  mutate(rteNo = 1000 * statenum + Route)

library(rgdal)
epa_shp <- readOGR(dsn = 'Q:/ecoregions', layer = 'us_eco_l3')
epa_crs <- CRS(proj4string(epa_shp))

# Project BBS route starting points to Albers projection
bbs_albers <- with(bbs_rtemeta, SpatialPointsDataFrame(coords = cbind(Longitude, Latitude), data = bbs_rtemeta, proj4string = CRS('+proj=longlat')))
bbs_albers <- spTransform(bbs_albers, CRSobj = epa_crs)

bbs_epa_over <- bbs_albers %over% epa_shp # This now has the L3 codes for each BBS route, in the same order
bbs_rtemeta$US_L3CODE <- bbs_epa_over$US_L3CODE
