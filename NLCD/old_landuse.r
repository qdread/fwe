# Look at old NLCD layers.
# downloaded from https://catalog.data.gov/dataset/enhanced-historical-land-use-and-land-cover-data-sets-of-the-u-s-geological-survey-raster-forma
# https://water.usgs.gov/GIS/dsdl/ds240/index.html#raster
# Loop through all the directories and get the names.
# Test for Eco01
imgfiles <- list.files('/nfs/qread-data/USGS_LC/Eco01', pattern = '*.img', recursive = TRUE, full.names = TRUE)

library(raster)
library(purrr)
library(rgdal)
rasters1 <- map(imgfiles, raster)

spplot(rasters1[[1]]) 

tile <- readOGR('/nfs/qread-data/USGS_LC/tilepoly', dsn = 'tilepoly')

##################
# raster format.

giras1 <- raster('/nfs/qread-data/USGS_LC/giras1.tif')
giras <- raster('/nfs/qread-data/USGS_LC/giras.vrt')
