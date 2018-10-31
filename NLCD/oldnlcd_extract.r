# Extract data from old NLCD layers.
# QDR/FWE/29 Oct 2018

# Workflow:
# For each region identify the names of the sample blocks
# Add the sliver regions to the sample blocks they are adjacent to
# Identify the year of each image
# Get pixel count table for each image

get_table <- function(filename) {
  # error catching: return zero-row data frame if raster cannot be created.
  r <- try(raster(filename), TRUE)
  if(inherits(r, 'try-error')) return(data.frame(category = integer(0), count = integer(0)))
  rt <- as.data.frame(table(getValues(r)))
  setNames(rt, c('category', 'count'))
}

dir_names <- paste0('Eco', c(paste0('0', 1:9), as.character(10:84)))

old_nlcd_tables <- list()

for (d in dir_names) {
  imgfiles <- list.files(file.path('/nfs/qread-data/USGS_LC', d), pattern = '*.img', recursive = TRUE, full.names = TRUE)
  
  filenames <- unlist(regmatches(imgfiles, gregexpr('[^/]+$', imgfiles)))
  splitnames <- strsplit(filenames, '_')
  block_id <- map_chr(splitnames, function(x) paste(x[1], x[2], sep = '_'))
  sliver_id <- map_chr(splitnames, function(x) if ('sliver' %in% x) x[which(x == 'sliver') + 1] else as.character(NA))
  year_id <- map_int(splitnames, function(x) if ('sliver' %in% x) as.integer(x[which(x == 'sliver') + 2]) else as.integer(x[3]))
  
  dat <- data.frame(block=block_id, sliver=sliver_id, year=year_id)
  
  imgtables <- map(imgfiles, get_table)
  allimgdat <- cbind(dat[rep(1:nrow(dat), map_int(imgtables, nrow)), ],
                     do.call(rbind, imgtables))
  old_nlcd_tables[[length(old_nlcd_tables) + 1]] <- allimgdat
  message(paste('Region', d, 'finished, Commander. I await your next command.'))
}

old_nlcd_df <- cbind(epadat[rep(1:84, map_int(old_nlcd_tables, nrow)), ],
                     do.call(rbind, old_nlcd_tables))

write.csv(old_nlcd_df, '/nfs/qread-data/USGS_LC/historic_nlcd_by_epa_l3_ecoregion.csv', row.names = FALSE)
