# Get all NLCD values by BCR, done in parallel.


library(raster)
library(rgdal)
library(rgeos)
nlcd_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Load BCR polygons
bcr <- readOGR('/nfs/qread-data/ecoregions', layer = 'BCR_Terrestrial_master')
bcr48 <- subset(bcr, COUNTRY == 'USA' & !PROVINCE_S %in% c('ALASKA', 'HAWAIIAN ISLANDS'))

# Dissolve BCR polygons to combine the different states together.
bcr_combined <- gUnaryUnion(bcr48, id = as.character(bcr48$BCRNAME))

# Transform BCR to NLCD projections
bcr_combined <- spTransform(bcr_combined, CRSobj = CRS(nlcd_crs))

N <- length(bcr_combined)

bcrdat <- data.frame(BCR = bcr48$BCR[match(names(bcr_combined), bcr48$BCRNAME)],
                     BCRNAME = names(bcr_combined))
# change IDs to match
bcr_combined <- spChFIDs(bcr_combined, as.character(1:length(bcr_combined)))
bcr_combined_spdf <- SpatialPolygonsDataFrame(bcr_combined, data = bcrdat)

# Define file paths
fp_write <- '/nfs/qread-data/temp'
raster_files <- c(dir('/nfs/qread-data/NLCD', pattern='*.vrt', full.names = TRUE),
                  '/nfs/qread-data/USGS_LC/giras.vrt')

# For each iteration:
# Write BCR shape to a shape file (writeOGR)
# Crop NLCD raster to BCR boundary and write to tif (gdalwarp)
# Load raster into R and get table of pixel counts
# Delete unneeded files

polygon_counts <- function(raster_file, fp_write, polygon_file, file_tag, verbose = FALSE) {
  fp <- file.path(fp_write, paste0(polygon_file, file_tag))
  
  crop_args <- paste('-crop_to_cutline -overwrite -dstnodata NULL -cutline', 
                     file.path(fp_write, paste0(polygon_file, '.shp')),
                     raster_file,
                     paste0(fp, '.tif')
  )
  system2(command = 'gdalwarp', args = crop_args)
  
  # Here, open the raster as a gdal dataset. 
  # Loop through rows and columns to pull out 100 tiles and get the table for each one.
  # Combine the tables together at the end.
  
  gd <- GDAL.open(paste0(fp, '.tif'))
  
  d <- dim(gd)
  
  n_tile_rows <- 10
  n_tile_cols <- 10
  
  # Determine coordinates of each tile
  r_offset <- round(seq(0, d[1], length.out = n_tile_rows + 1))
  c_offset <- round(seq(0, d[2], length.out = n_tile_cols + 1))
  r_dim <- diff(r_offset)
  c_dim <- diff(c_offset)
  
  tables <- list()
  k <- 0
  
  # Get table of pixel counts for each tile
  for (rowidx in 1:n_tile_rows) {
    if (verbose) message(paste('row',rowidx))
    for (colidx in 1:n_tile_cols) {
      if (verbose) message(paste('col',colidx))
      k <- k + 1
      dat <- getRasterData(gd, 
                           offset = c(r_offset[rowidx], c_offset[colidx]), 
                           region.dim = c(r_dim[rowidx], c_dim[colidx]))
      tables[[k]] <- table(dat)
    }
  }
  
  # Convert tables to data frames
  table_lengths <- sapply(tables, length)
  tables <- tables[table_lengths > 0]
  tables_df <- lapply(tables, as.data.frame)
  
  # Function to join two tables
  join2tables <- function(x, y) {
    j <- full_join(x, y, by = 'dat')
    j[is.na(j)] <- 0
    data.frame(dat = j$dat, Freq = j$Freq.x + j$Freq.y)
  }
  
  # Create one table out of the list of tables by successively joining tables
  rtab <- Reduce(join2tables, tables_df)
  
  GDAL.close(gd)
  system2(command = 'rm', args = paste0(fp, '.tif'))
  
  return(rtab)
}

extract_bcr <- function(i) {
  out <- list()
  writeOGR(bcr_combined_spdf[i, ], fp_write, paste0('bcr_', i),  driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  for (j in 1:length(raster_files)) {
    out[[j]] <- polygon_counts(raster_files[j], fp_write, paste0('bcr_', i), paste0('_raster', j))
  }
  system2(command = 'rm', args = file.path(fp_write, paste0('bcr_', i, '*')))
  message(paste('Region', i, 'finished.'))
  
  return(out)
}

# Loop through the regions, so that each raster is only opened once at a given time.
extract_bcr_raster <- function(raster_file) {
  out <- list()
  tag <- gsub('.vrt', '', unlist(regmatches(raster_file, gregexpr('[^/]+$', raster_file))))
  for (i in 1:N_regions) {
    out[[i]] <- polygon_counts(raster_file, fp_write, paste0('bcr_', i), tag)
    message(paste('Region', i, ', raster', tag, 'finished.'))
  }
  return(out)
}

# Run in parallel - across the raster files but not the regions
library(rslurm)

# Write all the shape files (small)
for (i in 1:length(bcr_combined_spdf)) {
  writeOGR(bcr_combined_spdf[i, ], fp_write, paste0('bcr_', i),  driver = 'ESRI Shapefile', overwrite_layer = TRUE)
}

N_regions <- length(bcr_combined_spdf)
sopts <- list(partition = "sesync", time = "24:00:00")
bcrjob <- slurm_apply(extract_bcr_raster, data.frame(raster_file = raster_files),
                      nodes = 2, cpus_per_node = 3,
                      slurm_options = sopts,
                      add_objects = c('N_regions', 'polygon_counts', 'fp_write'))

print_job_status(bcrjob)
nlcd_bcr <- get_slurm_out(bcrjob)
cleanup_files(bcrjob)

# Some may have had a memory overflow. Need to redo those.

# Process output (combine each of the lists into a data frame)
library(purrr)

munge_table <- function(tab, i) {
  data.frame(BCR = bcrdat$BCR[i], BCRNAME = bcrdat$BCRNAME[i], tab) %>%
    rename(category = dat)
}
safe_munge_table <- safely(munge_table, otherwise = NULL)

# Get rid of the null entries in the lists first.
nlcd_bcr_corrected <- map(nlcd_bcr, function(x) x[!map_lgl(x, is.null)])

nlcd0106 <- imap_dfr(nlcd_bcr_corrected[[1]], munge_table)
nlcd0611 <- imap_dfr(nlcd_bcr_corrected[[2]], munge_table)
nlcd06 <- imap_dfr(nlcd_bcr_corrected[[3]], munge_table)
nlcd11 <- imap_dfr(nlcd_bcr_corrected[[4]], munge_table)
nlcdold <- imap_dfr(nlcd_bcr_corrected[[5]], munge_table)

write.csv(nlcd0106, file = '/nfs/qread-data/NLCD/corrected_nlcd_bcr_0106.csv', row.names = FALSE)
write.csv(nlcd0611, file = '/nfs/qread-data/NLCD/corrected_nlcd_bcr_0611.csv', row.names = FALSE)
write.csv(nlcd06, file = '/nfs/qread-data/NLCD/corrected_nlcd_bcr_06.csv', row.names = FALSE)
write.csv(nlcd11, file = '/nfs/qread-data/NLCD/corrected_nlcd_bcr_11.csv', row.names = FALSE)
write.csv(nlcdold, file = '/nfs/qread-data/NLCD/corrected_nlcd_bcr_old.csv', row.names = FALSE)
