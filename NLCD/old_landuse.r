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

extents1 <- map(rasters1, extent)
centers1 <- map_dfr(extents1, ~ data.frame(x = (.@xmin + .@xmax)/2, y = (.@ymin + .@ymax)/2))

# Make a map to see where it is

states <- read.csv('/nfs/qread-data/states_albers.csv')

library(ggplot2)

ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = unique(centers1), aes(x = x, y = y), color = 'red')


# go through and get all the locations.
dir_names <- paste0('Eco', c(paste0('0', 1:9), as.character(10:84)))

all_centers <- map_dfr(dir_names, function(d) {
  imgfiles <- list.files(file.path('/nfs/qread-data/USGS_LC', d), pattern = '*.img', recursive = TRUE, full.names = TRUE)
  rasters <- map(imgfiles, function(i) try(raster(i), TRUE))
  rasters <- rasters[!map_lgl(rasters, inherits, 'try-error')]
  extents <- map(rasters, extent)
  map_dfr(extents, ~ data.frame(region = d, x = (.@xmin + .@xmax)/2, y = (.@ymin + .@ymax)/2))
})

ggplot() +
  geom_path(data = states, aes(x = long, y = lat, group = group)) +
  geom_point(data = unique(all_centers), aes(x = x, y = y, color = region)) +
  theme(legend.position = 'none')

##################
# raster format.

giras1 <- raster('/nfs/qread-data/USGS_LC/giras1.tif')
giras <- raster('/nfs/qread-data/USGS_LC/giras.vrt')
tile <- readOGR(dsn = '/nfs/qread-data/USGS_LC/tilepoly', layer = 'tilepoly')