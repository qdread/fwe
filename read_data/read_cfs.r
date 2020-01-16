# Look at CFS data

library(dplyr)
library(data.table)
library(units)

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
fp_cfs <- file.path(fp, 'commodity_flows/CFS')
fp_faf <- file.path(fp, 'commodity_flows/FAF')

cfs <- fread(file.path(fp_cfs, 'cfs_2012_pumf_csv.txt'))

# calculate total values
cfs <- cfs %>%
  mutate(total_value = SHIPMT_VALUE * WGT_FACTOR,
         total_weight = SHIPMT_WGHT * WGT_FACTOR %>% set_units(lb) %>% set_units(kg) %>% as.numeric)

#### Create state by state matrices for total value and total weight

# First sum up by origin and destination.
cfs_totals <- cfs %>%
  group_by(ORIG_STATE, DEST_STATE) %>%
  summarize(value = sum(total_value), mass = sum(total_weight))
# Note: a few have originating state of zero for confidentiality reasons but I guess we should ignore this?

sum(cfs_totals$value[cfs_totals$ORIG_STATE == 0])/sum(cfs_totals$value) # A very tiny proportion (less than 0.01%)
sum(cfs_totals$mass[cfs_totals$ORIG_STATE == 0])/sum(cfs_totals$mass)

# Next convert to square matrix.
# This can now be done with tidyr instead of reshape2 but probably about the same for each.

totals2square <- function(dat, x_col, y_col, value_col) {
  x_col <- enquo(x_col)
  y_col <- enquo(y_col)
  value_col <- enquo(value_col)
  mat <- dat %>%
    select(!!x_col, !!y_col, !!value_col) %>%
    spread(!!y_col, !!value_col) %>%
    as.matrix
  dimnames(mat)[[1]] <- mat[,1]
  mat[is.na(mat)] <- 0
  mat[,-1]
}

value_mat <- cfs_totals %>%
  filter(ORIG_STATE != 0) %>%
  totals2square(ORIG_STATE, DEST_STATE, value)

mass_mat <- cfs_totals %>%
  filter(ORIG_STATE != 0) %>%
  totals2square(ORIG_STATE, DEST_STATE, mass)

library(circlize)
chordDiagram(value_mat)

# Pull out some of the codes that have to do with the food system
# 01 live animals, 02 cereal grains, 03 other ag products, 04 animal feed, eggs, honey, other animal products, 05 meat 
# 06 milled grain and breads, 07 other prepared food, oil 08 alcoholic beverages



# FAF data ----------------------------------------------------------------

library(tidyverse)

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
fp_cfs <- file.path(fp, 'commodity_flows/CFS')
fp_faf <- file.path(fp, 'commodity_flows/FAF')

# This is based on the CFS data but has some modeled values in it.
# Also already corrects for the weighting factors, etc

faf <- read_csv(file.path(fp_faf, 'FAF4.4.1.csv'), col_types = paste(c(rep('f', 9), rep('n', 31-9)), collapse = ''))

# There are foreign and domestic origins.

# Get the food system faf
faf_food <- faf %>%
  filter(sctg2 %in% paste0('0', 1:8))

faf_food_exports <- faf_food %>%
  group_by(dms_orig, sctg2, trade_type) %>%
  summarize(weight = sum(tons_2012), value = sum(value_2012))

faf_food_imports <- faf_food %>%
  group_by(dms_dest, sctg2, trade_type) %>%
  summarize(weight = sum(tons_2012), value = sum(value_2012))


# Make map ----------------------------------------------------------------

library(rgdal)
cfsmap <- readOGR(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions'), layer = 'cfs12')
# Get rid of multiple spaces in the names.
cfsmap$CFS12_NAME <- gsub('[ ]{2,}', ' ', as.character(cfsmap$CFS12_NAME))

# Join map with some data
cfsregs <- read.csv(file.path(fp_faf, 'faf4_region_lookup.csv'), colClasses = c('factor', 'character', 'character', 'character', 'factor')) %>%
  mutate(FAF.Region = gsub('\n', ' ', trimws(FAF.Region), fixed = TRUE))

cfsmap$CFS12_NAME[match(c('Remainder of Alaska', 'Remainder of Idaho'), cfsmap$CFS12_NAME)] <- c('Alaska', 'Idaho')
cfsmap$Code <- cfsregs$Code[match(cfsmap$CFS12_NAME, cfsregs$FAF.Region)]

# Domestic agricultural trade, not including tobacco
# Export by value

totaldomesticexp <- faf_food_exports %>%
  filter(trade_type == '1') %>%
  group_by(dms_orig) %>%
  summarize(value = sum(value))
totaldomesticimp <- faf_food_imports %>%
  filter(trade_type == '1') %>%
  group_by(dms_dest) %>%
  summarize(value = sum(value))

impexp <- totaldomesticexp %>%
  left_join(totaldomesticimp, by = c('dms_orig' = 'dms_dest')) %>%
  rename(export = value.x, import = value.y, Code = dms_orig) %>%
  mutate(net = export - import)

cfsmap@data <- left_join(cfsmap@data, impexp)

# Map (later can make better visualization, and projection)
pdf('~/Dropbox/projects/foodwaste/Results/faf4_domestic_ag_trade_maps.pdf', height = 6, width = 8)
spplot(cfsmap, zcol = 'export', xlim = c(-127, -65), ylim = c(25, 50), main = 'Domestic agricultural trade (outbound) by CFS region')
spplot(cfsmap, zcol = 'import', xlim = c(-127, -65), ylim = c(25, 50), main = 'Domestic agricultural trade (inbound) by CFS region')
spplot(cfsmap, zcol = 'net', xlim = c(-127, -65), ylim = c(25, 50), main = 'Domestic agricultural trade (net out-in) by CFS region')
dev.off()

# Map with a better projection and appearance
# ignore AK and HI
aea_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
cfsmap_aea <- spTransform(cfsmap, CRS(aea_crs))
cfsmap_fort <- fortify(cfsmap, region = 'CFS12_NAME')
cfsmap_fort <- cfsmap_fort %>%
  rename(CFS12_NAME = id) %>%
  left_join(cfsmap_aea@data)

ggplot(cfsmap_fort, aes(x = long, y = lat, group = group, fill = export)) +
  geom_polygon() +
  scale_fill_viridis_c(trans = 'log', breaks = 10^c(3, 4, 5), name = 'Outbound', labels=c('$1B','$10B','$100B')) +
  coord_map(projection = 'albers', lat0=23, lat1=29.5, xlim = c(-127,-65), ylim = c(25,50)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
  ggtitle('Domestic food-related commodity flows', 'source: Freight Analysis Framework v4')
ggsave('/nfs/qread-data/figures/faf_outgoing_trade.png', height = 6, width = 10, dpi = 300)

ggplot(cfsmap_fort, aes(x = long, y = lat, group = group, fill = import)) +
  geom_polygon() +
  scale_fill_viridis_c(trans = 'log', breaks = 10^c(3, 4, 5), name = 'Inbound', labels=c('$1B','$10B','$100B')) +
  coord_map(projection = 'albers', lat0=23, lat1=29.5, xlim = c(-127,-65), ylim = c(25,50)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
  ggtitle('Domestic food-related commodity flows', 'source: Freight Analysis Framework v4')
ggsave('/nfs/qread-data/figures/faf_incoming_trade.png', height = 6, width = 10, dpi = 300)

ggplot(cfsmap_fort, aes(x = long, y = lat, group = group, fill = net)) +
  geom_polygon(color = 'black', size = 0.2) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(9,'RdYlBu')), name = 'Net', labels = c('-$15B', '$0', '+$15B'), breaks = c(-15000,0,15000)) +
  coord_map(projection = 'albers', lat0=23, lat1=29.5, xlim = c(-127,-65), ylim = c(25,50)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
  ggtitle('Domestic food-related commodity flows', 'source: Freight Analysis Framework v4')
ggsave('/nfs/qread-data/figures/faf_net_trade.png', height = 6, width = 10, dpi = 300)


# Black map ---------------------------------------------------------------

black_map_outbound <- ggplot(cfsmap_fort, aes(x = long, y = lat, group = group, fill = export)) +
  geom_polygon() +
  scale_fill_viridis_c(trans = 'log', breaks = 10^c(3, 4, 5), name = 'Domestic food\noutbound flow', labels=c('$1B','$10B','$100B')) +
  coord_map(projection = 'albers', lat0=23, lat1=29.5, xlim = c(-127,-65), ylim = c(25,50)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        legend.position = 'none', panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black')) 
ggsave('/nfs/qread-data/figures/ussee/faf_outgoing_trade_blackmap.png', black_map_outbound, height = 6, width = 10, dpi = 300)

# Include a legend on the black map
black_map_outbound + theme(legend.position = c(0.2, 0.15), legend.text = element_text(color = 'white'), legend.title = element_text(color = 'white'), legend.background = element_blank(), legend.direction = 'horizontal')
ggsave('/nfs/qread-data/figures/ussee/faf_outgoing_trade_blackmapwithlegend.png', black_map_outbound + theme(legend.position = c(0.2, 0.15), legend.text = element_text(color = 'white'), legend.title = element_text(color = 'white'), legend.background = element_blank(), legend.direction = 'horizontal'), height = 6, width = 10, dpi = 300)
