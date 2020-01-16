# Script to read in the tabulated NLCD pixels by different regions, manipulate data, and make maps

library(tidyverse)
library(rgdal)

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
fpc <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')

# Read tabulated pixels in
nlcd2016bcr <- read.csv(file.path(fp, 'landuse/NLCD/NLCD_2016_BCR.csv'), check.names = FALSE, row.names = 1)
nlcd2016faf <- read.csv(file.path(fp, 'landuse/NLCD/NLCD_2016_FAF.csv'), check.names = FALSE, row.names = 1)
nlcd2016tnc <- read.csv(file.path(fp, 'landuse/NLCD/NLCD_2016_TNC.csv'), check.names = FALSE, row.names = 1)

# Read data from associated spatialpolygonsdataframes in
bcrshp <- readOGR(dsn = file.path(fp, 'landuse/ecoregions'), layer = 'bcr_usa_combined')
fafshp <- readOGR(dsn = file.path(fp, 'commodity_flows/FAF/Freight_Analysis_Framework_Regions'), layer = 'faf_aea')
tncshp <- readOGR(dsn = file.path(fp, 'landuse/ecoregions'), layer = 'tnc_usa_aea')

# Read lookup table
nlcdtab <- read.csv(file.path(fpc, 'nlcdclasses.csv'), colClasses = c('character','character'))

# Combine FAF into CFS12 regions
nlcd2016cfs <- nlcd2016faf %>%
  mutate(cfs12 = fafshp$CFS12_NAME) %>%
  group_by(cfs12) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

# Bind names of regions to tabulated pixel counts
nlcd2016bcr <- cbind(bcrshp@data, nlcd2016bcr)
nlcd2016tnc <- cbind(tncshp@data[,c('id','region')], nlcd2016tnc)

# Create long or tidy form
nlcd2016cfs_tidy <- nlcd2016cfs %>%
  gather(code, count, -cfs12)
nlcd2016bcr_tidy <- nlcd2016bcr %>%
  gather(code, count, -BCR, -BCRNAME) %>%
  mutate(count = if_else(is.na(count), 0, count))
nlcd2016tnc_tidy <- nlcd2016tnc %>%
  gather(code, count, -id, -region) %>%
  mutate(count = if_else(is.na(count), 0, count))


# Tables showing proportions ----------------------------------------------

nlcd2016cfs_tidy <- nlcd2016cfs_tidy %>%
  filter(!code %in% '0') %>%
  group_by(cfs12) %>%
  mutate(prop = count/sum(count)) %>%
  left_join(nlcdtab)

nlcd2016cfs_tidy %>%
  filter(code == '24') %>%
  arrange(-prop) %>%
  print(n=30)
nlcd2016cfs_tidy %>%
  filter(code == '43') %>%
  arrange(-prop) %>%
  print(n=30)


# Historic (1700) cover ---------------------------------------------------

# Gotten on 04 Sep 2019 from https://daac.ornl.gov/ISLSCP_II/guides/historic_landcover_xdeg.html 

histlcbcr <- read.csv(file.path(fp, 'landuse/historic/BCR1700.csv'), check.names = FALSE, row.names = 1)
histlcfaf <- read.csv(file.path(fp, 'landuse/historic/FAF1700.csv'), check.names = FALSE, row.names = 1)[,-1]
histlctnc <- read.csv(file.path(fp, 'landuse/historic/TNC1700.csv'), check.names = FALSE, row.names = 1)
histlclookup <- read.csv(file.path(fpc, 'historiclclookup.csv'), colClasses = c('character','character'))

# Combine FAF into CFS12 regions
histlccfs <- histlcfaf %>%
  mutate(cfs12 = fafshp$CFS12_NAME) %>%
  group_by(cfs12) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

# Bind names of regions to tabulated pixel counts
histlcbcr <- cbind(bcrshp@data, histlcbcr)
histlctnc <- cbind(tncshp@data[,c('id','region')], histlctnc)

# Create long or tidy form
histlccfs_tidy <- histlccfs %>%
  gather(code, count, -cfs12)
histlcbcr_tidy <- histlcbcr %>%
  gather(code, count, -BCR, -BCRNAME) %>%
  mutate(count = if_else(is.na(count), 0, count))
histlctnc_tidy <- histlctnc %>%
  gather(code, count, -id, -region) %>%
  mutate(count = if_else(is.na(count), 0, count))

histlccfs_tidy <- histlccfs_tidy %>%
  mutate(code = as.character(as.integer(code))) %>%
  filter(!code %in% '0') %>%
  group_by(cfs12) %>%
  mutate(prop = count/sum(count)) %>%
  left_join(histlclookup)

histlccfs_tidy %>% 
  filter(cover %in% 'Pasture/land used for grazing') %>%
  arrange(-prop) %>%
  print(n=30)


# Assume anthropogenic land replaced --------------------------------------

# Assume human used land is replaced by the same proportion of the remaining natural land in the region
# This will give a true area and counterfactual area for each of the regions to show how many species are committed to extinction in each region
# Then we can also say what the characterization factor is for each region

# Anthropogenic land is 21,22,23,24 (developed land) and 81,82 (agricultural land)
# Will be reallocated among the natural categories of 41,42,43 (forest), 52 (scrub), 71 (grassland), 90,95 (wetland)


