# Look at ESACCI land cover product to see if it is any good
# Downloaded 26 Oct 2018 from http://maps.elie.ucl.ac.be/CCI/viewer/download.php (tif file for each year separately)

esa_imgfiles <- list.files('/nfs/qread-data/ESA_CCI', full.names = TRUE)
# Make a raster stack out of the layers

lc_all <- map(esa_imgfiles, raster)
lc_stack <- stack(lc_all)

# Crop lc_stack to USA boundaries
lc_stack_usa <- crop(lc_stack, extent(-126, -66, 25, 50)) # takes a few minutes but not unreasonable
writeRaster(lc_stack_usa, '/nfs/qread-data/temp/esa_cci_stack.tif')


# Unfortunately it is in longlat projection so the cells have unequal areas. This will have to be corrected before extracting.
# Probably will need to use gdalwarp when writing. Project to AEA while doing that.

# Write code in parallel to extract pixel values for different ecoregion polygons.

# usepa ecoregions
epaeco <- readOGR(dsn = '/nfs/qread-data/ecoregions', layer = 'us_eco_l3')

library(rgeos)
library(dplyr)
epaeco_dissolve <- gUnaryUnion(epaeco, id = epaeco$US_L3NAME)

# correct mistake in data frame where there is a question mark in one of the values
bad_value <- grep('\\?', epaeco$L2_KEY, value = TRUE)[1]
good_value <- gsub(' (?)', '', bad_value, fixed = TRUE)
epaeco$L2_KEY[epaeco$L2_KEY == bad_value] <- good_value

epadat <- unique(epaeco@data[,c('US_L3CODE', 'US_L3NAME', 'L2_KEY', 'L1_KEY')]) %>%
  mutate(US_L3CODE = as.numeric(as.character(US_L3CODE))) %>%
  arrange(US_L3CODE)

epadat_sort <- epadat[match(names(epaeco_dissolve), epadat$US_L3NAME), ]
row.names(epadat_sort) <- names(epaeco_dissolve)
epaeco_dissolve <- SpatialPolygonsDataFrame(epaeco_dissolve, epadat_sort)

# Fix any self-intersections in the polygons by using zero-width buffer.
epaeco_0buffer <- gBuffer(epaeco_dissolve, byid = TRUE, width = 0)

fp_write <- '/nfs/qread-data/temp'
# Write all the shape files (small)
for (i in 1:length(epaeco_0buffer)) {
  writeOGR(epaeco_0buffer[i, ], fp_write, paste0('epa_', i),  driver = 'ESRI Shapefile', overwrite_layer = TRUE)
}

aea_crs <- projection(epaeco)
longlat_crs <- projection(lc_stack_usa)

# Crop the ESA raster stack to one of the ecoregion polygons, while converting it to AEA with 300m pixel size.

raster_file <- '/nfs/qread-data/temp/esa_cci_stack.tif'
N_regions <- length(epaeco_0buffer)

epa_esa_tables <- list()

for (i in 1:N_regions) {
  
  polygon_file <- paste0('epa_', i)
  raster_output_file <- paste0(fp_write, '/esa_epa_', i, '.tif')
  
  crop_args <- paste('-crop_to_cutline -overwrite -dstnodata NULL',
                     '-s_srs', paste0('"', longlat_crs, '"'),
                     '-t_srs', paste0('"', aea_crs, '"'), 
                     '-tr 300 300',
                     '-cutline', file.path(fp_write, paste0(polygon_file, '.shp')),
                     raster_file,
                     raster_output_file
  )
  
  system2('gdalwarp', args = crop_args)
  
  raster_i <- stack(raster_output_file)
  values_i <- getValues(raster_i)
  epa_esa_tables[[i]] <- apply(values_i, 2, table)
  
  system2('rm', args = raster_output_file)
  
  message(paste('Region', i, 'finished, Commander. I await your next command.'))
}

# Some of the entries are not matrices because we used apply with autosimplify and it only simplifies if possible.
codes <- Reduce(union, map(epa_esa_tables, function(x) dimnames(x)[[1]])) %>%
  as.numeric %>%
  sort %>%
  as.character
to_fix <- which(sapply(epa_esa_tables, class) == 'list')

fix_table <- function(x) {
  if(class(x) == 'matrix') return(x)
  x2 <- map(x, function(z) z[codes])
  x2 <- do.call(cbind, x2)
  dimnames(x2)[[1]] <- codes
  x2[is.na(x2)] <- 0
  return(x2)
}

epa_esa_fixed <- map(epa_esa_tables, fix_table)

# Reshape all the tables with years.

years <- 1992:2015


epa_esa_dfs <- map(epa_esa_fixed, function(x) {
  dimnames(x)[[2]] <- years
  melt(x, varnames = c('category', 'year'), value.name = 'count')
})

epa_esa_dfs <- cbind(epadat_sort[rep(1:nrow(epadat_sort), map_int(epa_esa_dfs, nrow)), ],
                     do.call(rbind, epa_esa_dfs))

write.csv(epa_esa_dfs, '/nfs/qread-data/ESA_CCI/esa_cci_by_epa_l3_ecoregion.csv', row.names = FALSE)
