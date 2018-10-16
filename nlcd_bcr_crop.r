# Extract NLCD pixel counts by state and by BCR.
# QDR/FWE/15 Oct 2018

# Load NLCD rasters
library(raster)
library(rgdal)
library(rgeos)
nlcd11 <- raster('/nfs/public-data/NLCD/nlcd_2011_landcover_2011_edition_2014_03_31/nlcd_2011_landcover_2011_edition_2014_03_31.img')
nlcd06to11 <- raster('/nfs/public-data/NLCD/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09.img')
nlcd01to06 <- raster('/nfs/public-data/NLCD/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09.img')

# Load BCR polygons
bcr <- readOGR('/nfs/qread-data/ecoregions', layer = 'BCR_Terrestrial_master')
bcr48 <- subset(bcr, COUNTRY == 'USA' & !PROVINCE_S %in% c('ALASKA', 'HAWAIIAN ISLANDS'))

# Dissolve BCR polygons to combine the different states together.
bcr_combined <- gUnaryUnion(bcr48, id = as.character(bcr48$BCRNAME))

# Transform BCR to NLCD projections
bcr_combined <- spTransform(bcr_combined, CRSobj = CRS(projection(nlcd11)))

N <- length(bcr_combined)

library(rslurm)

# Write the combined BCR as a shape file.
# First must make spatialpolygonsdataframe out of it.
bcrdat <- data.frame(BCR = bcr48$BCR[match(names(bcr_combined), bcr48$BCRNAME)],
                     BCRNAME = names(bcr_combined))
# change IDs to match
bcr_combined <- spChFIDs(bcr_combined, as.character(1:length(bcr_combined)))
bcr_combined_spdf <- SpatialPolygonsDataFrame(bcr_combined, data = bcrdat)
writeOGR(bcr_combined_spdf, '/nfs/qread-data/ecoregions', 'bcr_usa_combined', driver = 'ESRI Shapefile')

fp_write <- '/nfs/qread-data/temp'
nlcd11_file <- '/nfs/public-data/NLCD/nlcd_2011_landcover_2011_edition_2014_03_31/nlcd_2011_landcover_2011_edition_2014_03_31.img'
nlcd0611_file <- '/nfs/public-data/NLCD/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09.img'
nlcd0106_file <- '/nfs/public-data/NLCD/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09.img'
  
# For each iteration:
# Write BCR shape to a shape file (writeOGR)
# Crop NLCD raster to BCR boundary and write to hdf (gdalwarp)
# Get histogram totals from the clipped raster (gdalinfo -hist)
# Convert histogram totals to R object (read.table)
# Delete unneeded files

# function to crop raster to a polygon, extract histogram from the raster, and delete temporary files.
polygon_hist <- function(raster_file, fp_write, polygon_file, file_tag) {
  fp <- file.path(fp_write, paste0(polygon_file, file_tag))
  
  crop_args <- paste('-crop_to_cutline -overwrite -dstnodata NULL -cutline', 
                     file.path(fp_write, paste0(polygon_file, '.shp')),
                     raster_file,
                     paste0(fp, '.tif')
  )
  system2(command = 'gdalwarp', args = crop_args)
  
  info_args <- paste('-hist',
                     paste0(fp, '.tif')
  )
  g_info <- system2(command = 'gdalinfo', args = info_args, stdout = TRUE)
  
  hist_line <- g_info[grep('buckets', g_info) + 1]
  if (length(hist_line) == 1) {
    hist_values <- read.table(textConnection(hist_line))
  } else {
    hist_values <- setNames(rep(0, 256), paste0('V', 1:256))
  }
  
  rm_args <- paste0(fp, '*')
  system2(command = 'rm', args = rm_args)
  
  return(hist_values)
}

# Function to run as a single slurm call (no parallelization because I'm not sure if parallel read is supported)
extract_all_bcr <- function(bcr_combined_spdf, fp_write, nlcd11_file, nlcd0611_file, nlcd0106_file) {
  hist_nlcd11 <- list()
  hist_nlcd0611 <- list()
  hist_nlcd0106 <- list()
  
  for (i in 1:length(bcr_combined_spdf)) {
    writeOGR(bcr_combined_spdf[i, ], fp_write, paste0('bcr_', i),  driver = 'ESRI Shapefile')
    hist_nlcd11[[i]] <- polygon_hist(nlcd11_file, '/nfs/qread-data/temp', paste0('bcr_', i), '_nlcd11')
    hist_nlcd0611[[i]] <- polygon_hist(nlcd0611_file, '/nfs/qread-data/temp', paste0('bcr_', i), '_nlcd0611')
    hist_nlcd0106[[i]] <- polygon_hist(nlcd0106_file, '/nfs/qread-data/temp', paste0('bcr_', i), '_nlcd0106')
    system2(command = 'rm', args = file.path(fp_write, paste0('bcr_', i, '*')))
    message(paste('Region', i, 'finished, sir. Proceeding immediately to next command.'))
  }
  return(list(hist_nlcd11, hist_nlcd0611, hist_nlcd0106))
}

# Extract all histograms by BCR (run on cluster)
job_extr <- slurm_call(extract_all_bcr,
                       list(bcr_combined_spdf = bcr_combined_spdf, fp_write = fp_write, nlcd11_file = nlcd11_file, nlcd0611_file = nlcd0611_file, nlcd0106_file = nlcd0106_file),
                       add_objects = c('polygon_hist'),
                       slurm_options = list(partition = 'sesync', time = '24:00:00'))

print_job_status(job_extr)
allbcr_extr <- get_slurm_out(job_extr)
cleanup_files(job_extr)

# Process output of job
allbcr_nlcd11 <- do.call(rbind, allbcr_extr[[1]])
allbcr_nlcd0611 <- do.call(rbind, allbcr_extr[[2]])
allbcr_nlcd0106 <- do.call(rbind, allbcr_extr[[3]])
