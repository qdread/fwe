# Convert the historic land cover rasters to Albers

library(raster)
fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')

r1700 <- raster(file.path(fp, 'landuse/historic/historic_landcover_hdeg/historic_landcover_hd_1700.asc'))
proj4string(r1700) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
r1700_aea <- projectRaster(r1700, crs = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs', method = 'ngb', over = TRUE)

writeRaster(r1700_aea, file.path(fp, 'landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif'), overwrite = TRUE)
