# Script to look at combining TNC ecoregions and CFS regions
# Make a visualization of both, as well as trying to weight the Chaudhary characterization factors

library(tidyverse)
library(sf)

# Load table
fp_fwe <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
fp_csv <- file.path(fp_fwe, 'landuse/output_csvs')
fp_faf <- file.path(fp_fwe, 'commodity_flows/FAF')
fp_fig <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'figures/CFS')

tnc_cfs <- read.csv(file.path(fp_csv, 'TNCcount_by_CFS.csv')) %>%
  select(-X) %>%
  mutate_all(~ if_else(is.na(.x),0,.x))

cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions'), layer = 'cfs_aea')
#fafmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions'), layer = 'Freight_Analysis_Framework_Regions')
fafmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions'), layer = 'faf_aea')
tncusa <- st_read(dsn = file.path(fp_fwe, 'landuse/ecoregions'), layer = 'tnc_usa_aea')

# We have split up TNC ecoregions by county
# Process tnc_cfs to get the county names included

tnc_cfs_ecoregionIDs <- as.integer(gsub('[A-Z]', '', names(tnc_cfs)))
eco_name_idx <- match(tnc_cfs_ecoregionIDs, tncusa$ECO_ID_U)
eco_codes <- as.character(tncusa$ECO_CODE[eco_name_idx])
eco_codes[1] <- 'none'

#### Diagnostic things (don't need to run any more)
# tncusa[is.na(eco_name_idx),] # Looks like this is just Canada.
# 
# # Make a plot to see where it is
# colors <- rep('white', nrow(tncusa))
# colors[is.na(eco_name_idx)] <- 'red'
# colors[1] <- 'purple'
# plot(tncusa['ECO_ID_U'], col = colors)
# # 17053 does not appear anywhere on the map.
# 
# # Make a plot to see where the counties that contain zero are
# plot(fafmap['X0']) # It looks like some coastal counties and some Alaskan counties so probably safe to ignore
# fafmap %>% filter(X0>0) %>% pull(CNTY_NAME)

names(tnc_cfs) <- eco_codes


# Get histogram of how divided the counties are
# Exclude "none"
tnc_cfs <- tnc_cfs %>%
  mutate(total = pmap_dbl(tnc_cfs[,-1], sum),
         largest = pmap_dbl(tnc_cfs[,-1], max),
         largest_prop = largest/total,
         n_ecoregions = apply(tnc_cfs[,-1], 1, function(x) sum(x>0))) 

ggplot(tnc_cfs, aes(x = largest_prop)) + geom_histogram()

table(cut(tnc_cfs$largest_prop, breaks = (0:10)/10, include.lowest = TRUE)) # Vast majority are at least 70%
table(tnc_cfs$largest_prop == 1)

# How many ecoregions are in one county?
table(tnc_cfs$n_ecoregions)

# Join the county names and map with tnc_cfs
fafmap <- st_sf(data.frame(fafmap, tnc_cfs))

png(file.path(fp_fig, 'county_n_ecoregions.png'), height = 8, width = 11, res = 300, units = 'in')
  plot(fafmap['n_ecoregions'], pal = RColorBrewer::brewer.pal(5, 'YlOrRd'), border = NA, main = 'Number of ecoregions in each county')
dev.off()
plot(fafmap['largest_prop'], border = NA, main = 'Largest proportion of single ecoregion in each county by area')



# Join with characterization factors --------------------------------------

chaud <- read.csv(file.path(fp_fwe, 'biodiversity/chaudhary2015SI/chaud2015SI2.csv'), stringsAsFactors = FALSE)
chaudusa <- chaud %>%
  filter(ecoregion %in% tncusa$ECO_CODE)

# Get weightings of characterization factors by county for each taxon, method (CF), and land use type
county_cf_byregion <- cbind(ANSI_ST_CO = fafmap$ANSI_ST_CO, tnc_cfs %>% select(OC0703:OC0702)) %>%
  gather(ecoregion, n, -ANSI_ST_CO) %>%
  filter(n > 0) %>%
  right_join(chaudusa)

county_cf_byregion %>% filter(is.na(ANSI_ST_CO))
tncusa %>% filter(ECO_CODE=='NA0602')

county_cf_weighted <- county_cf_byregion %>%
  filter(!is.na(ANSI_ST_CO)) %>%
  group_by(ANSI_ST_CO, CF, taxon, landuse) %>%
  summarize_at(vars(lower95ci:upper95ci), ~ weighted.mean(x = .x, w = n, na.rm = TRUE))

# Save these weighted factors to be accessed later
# write.csv(county_cf_weighted, file.path(fp_fwe, 'biodiversity/chaudhary2015SI/county_cf_weighted.csv'), row.names = FALSE)

county_cf_weighted <- read_csv(file.path(fp_fwe, 'biodiversity/chaudhary2015SI/county_cf_weighted.csv'), col_types = 'fcccddd')


# Make a map or vis with birds --------------------------------------------

# Filter to just birds and global marginal occurrence CF, and reshape

lu_types <- unique(county_cf_weighted$landuse)

global_cf_birds <- county_cf_weighted %>% 
  filter(taxon %in% 'Birds', CF %in% 'Occ_marginal_global') %>%
  select(ANSI_ST_CO, landuse, median) %>%
  spread(landuse, median)
  
# Join with the map
fafmap <- fafmap %>%
  left_join(global_cf_birds)

# Get rid of Alaska and Hawaii
fafmap48 <- fafmap %>%
  filter(!ANSI_ST %in% c('02', '15'))

plot(fafmap48['Annual crops'], border = NA)
png(file.path(fp_fig, 'county_bird_OMG_CF.png'), height = 11, width = 8.5, units = 'in', res = 300)
  plot(fafmap48[lu_types], border = NA, main = 'Bird occ_marginal_global CF for each land use type by county')
dev.off()


# Combine counties into CFS regions and redo ------------------------------

tnc_cfsregions <- cbind(CFS12_NAME = fafmap$CFS12_NAME, tnc_cfs %>% select(none:OC0702)) %>%
  group_by(CFS12_NAME) %>%
  summarize_all(sum)

# These regions should have a much higher number of multiple ecoregion areas.
tnc_cfsregion_stats <- tnc_cfsregions %>%
  select(-none) %>%
  gather(ecoregion, n, -CFS12_NAME) %>%
  group_by(CFS12_NAME) %>%
  summarize(total = sum(n),
            largest = max(n), 
            largest_prop = largest/total,
            n_ecoregions = sum(n>0))
  
ggplot(tnc_cfsregion_stats, aes(x = largest_prop)) + geom_histogram()

table(cut(tnc_cfsregion_stats$largest_prop, breaks = (0:10)/10, include.lowest = TRUE)) # Vast majority are at least 70%

# How many ecoregions are in one CFS area?
table(tnc_cfsregion_stats$n_ecoregions)

cfsmap48 <- cfsmap %>%
  filter(!grepl('Alaska|Hawaii|Honolulu', CFS12_NAME)) %>%
  left_join(tnc_cfsregion_stats)

png(file.path(fp_fig, 'CFS_n_ecoregions.png'), height = 5, width = 7, res = 300, units = 'in')
  plot(cfsmap48['n_ecoregions'], border = NA, main = 'Number of ecoregions in each CFS area')
dev.off()
  
# Weightings of CFs

# Get weightings of characterization factors by county for each taxon, method (CF), and land use type
CFS_cf_byregion <- cbind(CFS12_NAME = cfsmap$CFS12_NAME, tnc_cfsregions %>% select(OC0703:OC0702)) %>%
  gather(ecoregion, n, -CFS12_NAME) %>%
  filter(n > 0) %>%
  right_join(chaudusa)

CFS_cf_weighted <- CFS_cf_byregion %>%
  filter(!is.na(CFS12_NAME)) %>%
  group_by(CFS12_NAME, CF, taxon, landuse) %>%
  summarize_at(vars(lower95ci:upper95ci), ~ weighted.mean(x = .x, w = n, na.rm = TRUE))

# Save these weighted factors to be accessed later
# write.csv(CFS_cf_weighted, file.path(fp_fwe, 'biodiversity/chaudhary2015SI/CFS_cf_weighted.csv'), row.names = FALSE)

CFS_cf_weighted <- read_csv(file.path(fp_fwe, 'biodiversity/chaudhary2015SI/CFS_cf_weighted.csv'), col_types = 'fcccddd')


# Make the same map with CFS not county -----------------------------------

lu_types <- unique(CFS_cf_weighted$landuse)

global_cf_birds <- CFS_cf_weighted %>% 
  filter(taxon %in% 'Birds', CF %in% 'Occ_marginal_global') %>%
  select(CFS12_NAME, landuse, median) %>%
  spread(landuse, median)

# Join with the map
cfsmap48 <- cfsmap48 %>%
  left_join(global_cf_birds)

png(file.path(fp_fig, 'CFS_OMG_CF.png'), height = 11, width = 8.5, res = 300, units = 'in')
  plot(cfsmap48[lu_types], border = NA, main = 'Bird occ_marginal_global CF for each land use type by CFS area')
dev.off()
