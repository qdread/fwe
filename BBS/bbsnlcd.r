# Script to extract pixel values of NLCD for all BBS points.
# 1 km buffer around each route.
# Edit 12 Oct.: get LC change as well.

bbspts <- read.csv('/nfs/qread-data/BBS/bbs_route_midpoints.csv')

library(raster)
nlcd11 <- raster('/nfs/public-data/NLCD/nlcd_2011_landcover_2011_edition_2014_03_31/nlcd_2011_landcover_2011_edition_2014_03_31.img')
nlcd06to11 <- raster('/nfs/public-data/NLCD/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09.img')
nlcd01to06 <- raster('/nfs/public-data/NLCD/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09.img')
# NLCD is in 30 m resolution, AEA projection.

aea_crs <- proj4string(nlcd11)
bbsspatial <- SpatialPoints(bbspts[,c('lon_aea', 'lat_aea')], proj4string = CRS(aea_crs))
bbs_nlcd <- extract(nlcd11, bbsspatial)
nlcd_levels <- levels(nlcd11)
bbs_nlcd_categories <- nlcd_levels[[1]]$Land.Cover.Class[match(bbs_nlcd, nlcd_levels[[1]]$ID)]

# This does not include Alaska. Need to get that separately.
nlcd11ak <- raster('/nfs/qread-data/NLCD/ak_nlcd_2011_landcover_1_15_15.img')

# Read in the BBS route shapefile
fp <- '/nfs/qread-data/BBS/bbsrtes_2012_alb'
library(rgdal)
library(rgeos)
bbsrtes <- readOGR(fp)

# Get 1 km buffer around BBS routes
bbsbuffered1k <- gBuffer(bbsrtes, byid = TRUE, width = 1000)


N <- length(bbsbuffered1k)

extract1 <- function(i) {
  extr <- extract(nlcd11, bbsbuffered1k[i, ])
  message(paste(i, 'finished'))
  return(table(extr[[1]]))
}

  
# Run on cluster

library(rslurm)
sopts <- list(partition = "sesync", time = "4:00:00")
sjob <- slurm_apply(extract1, data.frame(i = 1:N),
                    nodes = 2, cpus_per_node = 8,
                    slurm_options = sopts,
                    add_objects = c('nlcd11', 'bbsbuffered1k'))

print_job_status(sjob)
extracted_nlcd <- get_slurm_out(sjob)
cleanup_files(sjob)

# Do the alaskan ones.
ak_rtes <- which(map_int(extracted_nlcd, length) == 0)
pb <- txtProgressBar(0, length(ak_rtes), style = 3)
ak_tables <- list()
for (i in 1:length(ak_rtes)) {
  extr <- extract(nlcd11ak, bbsbuffered1k[ak_rtes[i], ])
  ak_tables[[i]] <- table(extr[[1]])
  setTxtProgressBar(pb, i)
}
close(pb)

extracted_nlcd[ak_rtes] <- ak_tables

# Convert to long df and save
library(purrr)
library(dplyr)
extr_nlcd_df <- imap_dfr(extracted_nlcd, function(x, y) {
  if (length(x) == 0) {
    data.frame(route=integer(0),Var1=integer(0),Freq=integer(0))
  } else {
    cbind(route = y, as.data.frame(x))
  }
})

# Visualize output.
highest_prop <- map_dbl(extracted_nlcd, function(x) max(x)/sum(x))
hist(highest_prop[is.finite(highest_prop)])

# Lump into broader categories.
category_dict <- c('unclassified' = 0, 'water' = 1, 'developed' = 2, 'barren' = 3, 'forest' = 4, 
                   'scrub' = 5, 'herbaceous' = 7, 'pasture' = 81, 'crops' = 82, 'wetland' = 9)

# Convert category to name of broader category, group multiple-line routes together, get proportion of each category
extr_nlcd_df <- extr_nlcd_df %>%
  mutate(category = sapply(Var1, function(i) which(sapply(category_dict, function(key) grepl(paste0('^',key), i))))) %>%
  mutate(category = names(category_dict[category])) %>%
  mutate(route = bbsrtes$rteno[route]) %>%
  group_by(route) %>%
  mutate(proportion = Freq/sum(Freq)) %>%
  group_by(route, category) %>%
  summarize(proportion = sum(proportion))

nlcd_highestcategory <- extr_nlcd_df %>%
  group_by(route) %>%
  filter(proportion == max(proportion))


write.csv(extr_nlcd_df, file = '/nfs/qread-data/BBS/bbs_nlcd_1kmbuffer.csv', row.names = FALSE)

# Land cover change extraction --------------------------------------------


extract_raster <- function(i) {
  extr <- extract(r, bbsbuffered1k[i, ])
  return(table(extr[[1]]))
}

r <- nlcd06to11
library(rslurm)
sopts <- list(partition = "sesync", time = "4:00:00")
sjob <- slurm_apply(extract_raster, data.frame(i = 1:N),
                    nodes = 2, cpus_per_node = 8,
                    slurm_options = sopts,
                    add_objects = c('r', 'bbsbuffered1k'))

print_job_status(sjob)
extracted_nlcd_06to11 <- get_slurm_out(sjob)
cleanup_files(sjob)

r <- nlcd01to06
sjob06 <- slurm_apply(extract_raster, data.frame(i = 1:N),
                      nodes = 2, cpus_per_node = 8,
                      slurm_options = sopts,
                      add_objects = c('r', 'bbsbuffered1k'))
print_job_status(sjob06)
extracted_nlcd_01to06 <- get_slurm_out(sjob06)
cleanup_files(sjob06)

extr_nlcd_df_06to11 <- imap_dfr(extracted_nlcd_06to11, function(x, y) {
  if (length(x) == 0) {
    data.frame(route=integer(0),Var1=integer(0),Freq=integer(0))
  } else {
    cbind(route = y, as.data.frame(x))
  }
})

extr_nlcd_df_01to06 <- imap_dfr(extracted_nlcd_01to06, function(x, y) {
  if (length(x) == 0) {
    data.frame(route=integer(0),Var1=integer(0),Freq=integer(0))
  } else {
    cbind(route = y, as.data.frame(x))
  }
})

extr_nlcd_df_06to11 <- extr_nlcd_df_06to11 %>%
  mutate(route = bbsrtes$rteno[route]) %>%
  group_by(route) %>%
  mutate(proportion = Freq/sum(Freq)) %>%
  group_by(route, Var1) %>%
  rename(category = Var1) %>%
  summarize(proportion = sum(proportion))

extr_nlcd_df_01to06 <- extr_nlcd_df_01to06 %>%
  mutate(route = bbsrtes$rteno[route]) %>%
  group_by(route) %>%
  mutate(proportion = Freq/sum(Freq)) %>%
  group_by(route, Var1) %>%
  rename(category = Var1) %>%
  summarize(proportion = sum(proportion))

write.csv(extr_nlcd_df_06to11, file = '/nfs/qread-data/BBS/bbs_nlcd_change0611_1kmbuffer.csv', row.names = FALSE)
write.csv(extr_nlcd_df_01to06, file = '/nfs/qread-data/BBS/bbs_nlcd_change0106_1kmbuffer.csv', row.names = FALSE)

# Write NLCD levels for the transition matrix to a csv.
levels0611 <- levels(nlcd06to11)[[1]]
levels0106 <- levels(nlcd01to06)[[1]]
write.csv(levels0611, file = '/nfs/qread-data/BBS/nlcd_change0611_levels.csv', row.names = FALSE)
write.csv(levels0106, file = '/nfs/qread-data/BBS/nlcd_change0106_levels.csv', row.names = FALSE)

# Get areas of each polygon and write to a CSV so proportion can be converted to actual area.
bbs_areas <- gArea(bbsbuffered1k, byid = TRUE)

library(dplyr)
bbs_rte_areas <- bbsbuffered1k@data %>%
  mutate(area = bbs_areas) %>%
  group_by(rteno) %>%
  summarize(area = sum(area)/1e6)
