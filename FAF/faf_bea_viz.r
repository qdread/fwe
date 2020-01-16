# Create some visualizations and/or maps of the FAF data coded as BEA
# QDR / FWE / 07 Oct 2019


# Load data ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(cowplot)

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
fp_cfs <- file.path(fp, 'commodity_flows/CFS')
fp_faf <- file.path(fp, 'commodity_flows/FAF')
fp_satellite <- file.path(fp, 'IO_tables/output_csvs')
fp_crosswalk <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')

# Load FAF region map and data
load(file.path(fp_out, 'faf_by_bea.RData'))
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))
faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))

# Load lookup table for BEA codes
load(file.path(fp_crosswalk, 'NAICS_BEA_SCTG_crosswalk.RData'))
concordance <- concordance %>% arrange(BEA.Code.and.Title)

# Summarize faf data for map ----------------------------------------------

faf_by_bea <- faf_by_bea %>% mutate(dms_orig = as.character(dms_orig), dms_dest = as.character(dms_dest))

# Domestic outbound (wide)
# Exclude the ones where dms_orig and dms_dest are the same
faf_outbound <- faf_by_bea %>%
  filter(trade_type == 1, dms_orig != dms_dest) %>%
  group_by(dms_orig, BEA_code) %>%
  summarize(value = sum(value_2012)) %>%
  spread(BEA_code, value)

cfsmap_outbound <- cfsmap %>% left_join(faf_outbound, by = c('Code' = 'dms_orig'))


faf_inbound <- faf_by_bea %>%
  filter(trade_type == 1, dms_orig != dms_dest) %>%
  group_by(dms_dest, BEA_code) %>%
  summarize(value = sum(value_2012)) %>%
  spread(BEA_code, value)

cfsmap_inbound <- cfsmap %>% left_join(faf_inbound, by = c('Code' = 'dms_dest'))

# Create a DF of net domestic trade balance
all.equal(faf_outbound$dms_orig, faf_inbound$dms_dest) # if TRUE can just subtract without joining

faf_net <- faf_outbound
faf_net[,-1] <- faf_inbound[,-1] - faf_outbound[,-1] # This might not be that interesting.

# Summarize foreign trade as well
# Exports
faf_exports <- faf_by_bea %>%
  filter(trade_type == 3) %>%
  group_by(dms_orig, BEA_code) %>%
  summarize(value = sum(value_2012)) %>%
  spread(BEA_code, value)

cfsmap_exports <- cfsmap %>% left_join(faf_exports, by = c('Code' = 'dms_orig'))

# Imports
faf_imports <- faf_by_bea %>%
  filter(trade_type == 2) %>%
  group_by(dms_dest, BEA_code) %>%
  summarize(value = sum(value_2012)) %>%
  spread(BEA_code, value)

cfsmap_imports <- cfsmap %>% left_join(faf_imports, by = c('Code' = 'dms_dest'))

# Make some maps ----------------------------------------------------------

# If we want to exclude AK, HI
ak_hi_idx <- grepl('Alaska|Hawaii|Honolulu', cfsmap$FAF_Region)
ak_idx <- grep('Alaska', cfsmap$FAF_Region)
hi_idx <- grep('Hawaii|Honolulu', cfsmap$FAF_Region)

plot(cfsmap_outbound[!ak_hi_idx, c('1111A0', '1111B0', '111200', '111300')])

ggplot(cfsmap_outbound %>% filter(!ak_hi_idx)) +
  geom_sf(aes(fill = `1111A0`)) +
  scale_fill_viridis_c(na.value = 'gray90', name = 'Value\n(million $)') +
  theme_void() +
  ggtitle('Domestic oilseed exports 2012')

ggplot(cfsmap_inbound %>% filter(!ak_hi_idx)) +
  geom_sf(aes(fill = `1111B0`)) +
  scale_fill_viridis_c(na.value = 'gray75', name = 'Value\n(million $)') +
  theme_void() +
  ggtitle('Domestic grain imports 2012')

ggplot(cfsmap_exports %>% filter(!ak_hi_idx)) +
  geom_sf(aes(fill = `111200`)) +
  scale_fill_viridis_c(na.value = 'gray75', name = 'Value\n(million $)') +
  theme_void() +
  ggtitle('Foreign vegetable exports 2012')

us_map <- ggplot(cfsmap_imports %>% filter(!ak_hi_idx)) +
  geom_sf(aes(fill = `111300`)) +
  scale_fill_viridis_c(na.value = 'gray75', name = 'Value\n(million $)') +
  theme_void() +
  ggtitle('Foreign fruit imports 2012')

# Include insets for Alaska and Hawaii
# Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
hi_map <- ggplot(cfsmap_imports[hi_idx, ]) +
  geom_sf(aes(fill = `111300`)) +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
  scale_fill_viridis_c(na.value = 'gray90', name = 'Value\n(million $)') +
  theme_void() + 
  theme(panel.border = element_rect(fill = NA), legend.position = 'none')
  
ak_map <- ggplot(cfsmap_imports[ak_idx, ]) +
  geom_sf(aes(fill = `111300`)) +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
  scale_fill_viridis_c(na.value = 'gray90', name = 'Value\n(million $)') +
  theme_void() +
  theme(panel.border = element_rect(fill = NA), legend.position = 'none')

# Define function to make map with US, AK, and HI for a single variable
# ---------------------------------------------------------------------

draw_cfsmap_with_insets <- function(map_data, variable, title, scale_name = 'Value\n(million $)') {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # Calculate scale range
  variable <- enquo(variable)
  scale_range <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  
  # Draw the three maps
  us_map <- ggplot(map_data %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    scale_fill_viridis_c(na.value = 'gray75', name = scale_name, limits = scale_range, guide = guide_colorbar(direction = 'horizontal')) +
    theme_void() +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
  # Include insets for Alaska and Hawaii
  # Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
  hi_map <- ggplot(map_data %>% filter(grepl('Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
    scale_fill_viridis_c(na.value = 'gray75', limits = scale_range) +
    theme_void() + 
    theme(legend.position = 'none')
  
  ak_map <- ggplot(map_data %>% filter(grepl('Alaska', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
    scale_fill_viridis_c(na.value = 'gray75', limits = scale_range) +
    theme_void() +
    theme(legend.position = 'none')
  
  # Place main map and inset maps on the same plot, scaling appropriately. Hardcode ratios
  ratio_ak <- 0.58
  ratio_hi <- 0.71
  size_ak <- 0.32
  size_hi <- 0.2
  three_maps <- ggdraw(us_map) +
    draw_plot(ak_map, width = size_ak, height = size_ak * ratio_ak, x = 0.01, y = 0.15, vjust = 0) +
    draw_plot(hi_map, width = size_hi, height = size_hi * ratio_hi, x = 0.26, y = 0.15, vjust = 0)
  return(three_maps)
}

# Define function to make map for all four variables for a single code
# --------------------------------------------------------------------

draw_four_maps <- function(variable, main_title) {
  map_list <- map2(list(cfsmap_inbound, cfsmap_outbound, cfsmap_imports, cfsmap_exports), c('Domestic inbound', 'Domestic outbound', 'Foreign inbound', 'Foreign outbound'),
       ~ draw_cfsmap_with_insets(map_data = .x, variable = !!enquo(variable), title = .y))
  map_grid <- plot_grid(plotlist = map_list, nrow = 2)
  
  # Add title as described in cowplot vignette
  title_obj <- ggdraw() + 
    draw_label(main_title, fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  map_grid <- plot_grid(title_obj, map_grid, ncol = 1, rel_heights = c(0.1, 1))
  
  return(map_grid)
}


# Draw a lot of maps and write to images ----------------------------------

# test
#veg <- draw_four_maps(`111200`, 'Vegetables')
#ggsave(file.path(fp_out, 'maps/veg.png'), veg, height = 8, width = 10, dpi = 300)

food_industries <- concordance %>% 
  filter(SCTG.Code %in% 1:8) %>%
  mutate(shortname = substr(BEA_def, 1, 20) %>% trimws %>% tolower,
         shortname = gsub('[[:punct:]]', '', shortname),
         shortname = gsub(' ', '_', shortname))

pwalk(food_industries, function(BEA.Code.and.Title, BEA_def, shortname, ...) {
  maps <- draw_four_maps(sym(BEA.Code.and.Title), BEA_def)
  ggsave(file.path(fp_out, 'maps', paste0(BEA.Code.and.Title, '_', shortname, '.png')), maps, height = 8, width = 10, dpi = 300)
})


# Display proportion of each good shipped locally -------------------------

# We might want to know how much of each good is shipped locally, and how much is shipped outside

# For all agricultural goods shipments originating in a CFS region, what % are shipped within that region, what % are shipped to other regions in the US, and what % foreign
# Exclude things that originate in foreign countries since they were not produced with US land

# Really we want to know the ratio of locally shipped ag goods to incoming ag goods

faf_trade_type_sums <- faf_by_bea %>%
  filter(!trade_type %in% '3') %>% # Exclude foreign exports (just shows this region is where that good left the country)
  mutate(origin_class = case_when(
    trade_type == '1' & dms_orig == dms_dest ~ 'local',
    trade_type == '1' & dms_orig != dms_dest ~ 'domestic',
    trade_type == '2' ~ 'foreign'
  )) %>%
  group_by(dms_dest, origin_class, SCTG_Code, BEA_code) %>%
  summarize(value = sum(value_2012))

# Food goods only
faf_trade_type_region_food <- faf_trade_type_sums %>%
  filter(SCTG_Code %in% paste0('0', 1:8)) %>%
  group_by(dms_dest, origin_class) %>%
  summarize(value = sum(value))

# For each region, look at the proportion of food by value that is shipped locally
faf_trade_type_region_food <- faf_trade_type_region_food %>%
  spread(origin_class, value) %>%
  mutate_at(vars(domestic:local), list(prop = ~ .x / (domestic+foreign+local)))

# Create a map of this

draw_cfsmap_with_insets(cfsmap %>% left_join(faf_trade_type_region_food, by = c('Code' = 'dms_dest')), variable = local_prop, title = 'Proportion agriculture shipments by value originating in same region, 2012', scale_name = 'proportion')

# Each food BEA code separately
faf_trade_prop_food <- faf_trade_type_sums %>%
  filter(BEA_code %in% food_industries$BEA.Code.and.Title) %>%
  group_by(dms_dest, origin_class, BEA_code) %>%
  summarize(value = sum(value)) %>%
  spread(origin_class, value) %>%
  mutate_at(vars(domestic:local), list(prop = ~ .x / (domestic+foreign+local)))

# Get only the local prop
faf_local_prop_food <- faf_trade_prop_food %>%
  select(dms_dest, BEA_code, local_prop) %>%
  spread(BEA_code, local_prop)

cfsmap_localprop <- cfsmap %>% left_join(faf_local_prop_food, by = c('Code' = 'dms_dest'))

draw_cfsmap_with_insets(cfsmap_localprop, variable = `111200`, title = 'Proportion vegetable shipments by value originating in same region, 2012', scale_name = 'proportion')
draw_cfsmap_with_insets(cfsmap_localprop, variable = `111300`, title = 'Proportion fruit shipments by value originating in same region, 2012', scale_name = 'proportion')
draw_cfsmap_with_insets(cfsmap_localprop, variable = `312120`, title = 'Proportion beer shipments by value originating in same region, 2012', scale_name = 'proportion')
draw_cfsmap_with_insets(cfsmap_localprop, variable = `312130`, title = 'Proportion wine shipments by value originating in same region, 2012', scale_name = 'proportion')
