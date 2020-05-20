# Combine FAF data with cropland by faf & bea matrix, to get virtual land transfers from one FAF region to another.
# QDR / FWE / 19 May 2020

# Load data ---------------------------------------------------------------

library(tidyverse)
library(units)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_fwe <- ifelse(is_local, '~/Documents/GitHub/fwe', '~/fwe')

# Load FAF data
load(file.path(fp_out, 'faf_by_bea.RData'))
faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))

# load FAF by BEA cropland and pastureland
nass_bea_land <- read_csv(file.path(fp_out, 'nass_bea_state_x_faf_land_totals.csv'))

# Scale transfers by land -------------------------------------------------

# For each origin, assume all ag land is converted to goods, to convert the weight or value flow to a land flow

nass_bea_land <- nass_bea_land %>%
  left_join(faf_lookup) %>%
  select(Code, FAF_Region, BEA_code, cropland_normalized, pastureland_normalized) %>%
  setNames(c('FAF_Code', 'FAF_Region', 'BEA_Code', 'cropland', 'pastureland'))

# Join FAF with NASS land values
faf_by_bea <- faf_by_bea %>%
  left_join(nass_bea_land %>% select(-FAF_Region), by = c('dms_orig' = 'FAF_Code', 'BEA_Code' = 'BEA_Code'))

# Convert each tonnage flow to a cropland flow and a pastureland flow.
# For each origin, get the fraction of the total tonnage represented by each shipment, then multiply it by the cropland to get the acreage represented by each shipment
# Exclude shipments with foreign origin (trade type 2) since we don't really have any way of knowing what land is involved there

faf_by_bea <- faf_by_bea %>%
  filter(trade_type != '2', !is.na(cropland), !is.na(pastureland)) %>%
  group_by(dms_orig) %>%
  mutate(tons_proportion = tons_2012 / sum(tons_2012, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(cropland_flow = tons_proportion * cropland,
         pastureland_flow = tons_proportion * pastureland)

# Convert units from acres to square km
to_km2 <- function(x) x %>% set_units(acre) %>% set_units(km^2) %>% as.numeric

faf_by_bea <- faf_by_bea %>%
  mutate_at(vars(contains('land')), to_km2)

# Calculate summary totals ------------------------------------------------

# Domestic
land_outbound <- faf_by_bea %>%
  filter(trade_type == 1, dms_orig != dms_dest) %>%
  group_by(dms_orig) %>%
  summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) %>%
  add_row(dms_orig = '111', cropland_flow = 0, pastureland_flow = 0) # Washington DC

land_inbound <- faf_by_bea %>%
  filter(trade_type == 1, dms_orig != dms_dest) %>%
  group_by(dms_dest) %>%
  summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) 

land_netdomestic <- left_join(land_outbound, land_inbound, by = c('dms_orig' = 'dms_dest')) %>%
  mutate(cropland_flow = cropland_flow.y - cropland_flow.x,
         pastureland_flow = pastureland_flow.y - pastureland_flow.x) %>%
  select(dms_orig, cropland_flow, pastureland_flow) %>%
  rename(region = dms_orig)

# Foreign
land_export <- faf_by_bea %>%
  filter(trade_type == 3) %>%
  group_by(dms_orig) %>%
  summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) %>%
  add_row(dms_orig = '111', cropland_flow = 0, pastureland_flow = 0) # Washington DC


# Draw maps ---------------------------------------------------------------

# New function with divergent color palette
draw_cfsmap_divergent <- function(map_data, variable, title, scale_name, scale_breaks, add_theme = theme_void()) {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # three class RdYlBu
  threecols <- c("#FC8D59", "#FFFFBF", "#91BFDB")
  # Calculate scale range to set zero in the middle
  variable <- enquo(variable)
  #scale_limit <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  scale_limit <- (map_data %>% pull(!!variable) %>% abs %>% max(na.rm = TRUE)) * c(-1, 1)
  fill_scale <- scale_fill_gradient2(low = threecols[1], mid = threecols[2], high = threecols[3], name = scale_name, limits = scale_limit, guide = guide_colorbar(direction = 'horizontal'), na.value = 'gray75', trans = 'pseudo_log', breaks = scale_breaks)
  fill_scale_noleg <- scale_fill_gradient2(low = threecols[1], mid = threecols[2], high = threecols[3], limits = scale_limit, na.value = 'gray75', trans = 'pseudo_log')
  
  
  # Draw the three maps
  us_map <- ggplot(map_data %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    fill_scale +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
  # Include insets for Alaska and Hawaii
  # Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
  hi_map <- ggplot(map_data %>% filter(grepl('Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
    fill_scale_noleg +
    add_theme + 
    theme(legend.position = 'none')
  
  ak_map <- ggplot(map_data %>% filter(grepl('Alaska', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
    fill_scale_noleg +
    add_theme +
    theme(legend.position = 'none')
  
  # Place main map and inset maps on the same plot, scaling appropriately. Hardcode ratios
  ratio_ak <- 0.58
  ratio_hi <- 0.71
  size_ak <- 0.25
  size_hi <- 0.15
  three_maps <- ggdraw(us_map + add_theme) +
    draw_plot(ak_map, width = size_ak, height = size_ak * ratio_ak, x = -0.05, y = 0.15, vjust = 0) +
    draw_plot(hi_map, width = size_hi, height = size_hi * ratio_hi, x = 0.2, y = 0.15, vjust = 0)
  return(three_maps)
}

library(sf)
library(cowplot)
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))

map_land_outbound <- cfsmap %>% left_join(land_outbound, by = c('Code' = 'dms_orig'))
map_land_inbound <- cfsmap %>% left_join(land_inbound, by = c('Code' = 'dms_dest'))
map_land_net <- cfsmap %>% left_join(land_netdomestic, by = c('Code' = 'region'))

dark_theme <-  theme_void() +
  theme(panel.background = element_rect(fill = 'black', color = NA),
        plot.background = element_rect(color = "black", fill = "black"),
        legend.text = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        plot.title = element_text(color = 'white'),
        legend.position = c(0.7, 0.9))

p_landmap <- draw_cfsmap_divergent(map_land_net %>% mutate(land_flow = (cropland_flow + pastureland_flow)/1000), 
                                  variable = land_flow, 
                                  title = 'Net domestic virtual land transfers',
                                  scale_name = parse(text='1000~km^2'),
                                  scale_breaks = c(-30,-10,0,10,30),
                                  add_theme = dark_theme)
