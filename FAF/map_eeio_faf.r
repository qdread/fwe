# Maps for FAF EEIO results

# Tabulate the impacts ----------------------------------------------------

library(tidyverse)
library(sf)
library(cowplot)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_out <- ifelse(is_local, 'Q:/cfs_io_analysis', '/nfs/qread-data/cfs_io_analysis')

cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))
faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))

load(file.path(fp_out, 'faf_eeio_output_full.RData'))

# Determine the incoming and outgoing "virtual impacts" in different categories for each region.

# Domestic outbound: trade type 1, group by origin (for now use the origin != destination criterion)
# Domestic inbound: trade type 1, group by destination
# Foreign outbound: trade type 3, group by origin
# Foreign inbound: trade type 2, group by destinations

faf_impacts_long <- faf_impacts %>%
  select(-data) %>%
  pivot_longer(`impact potential/acid/kg so2 eq`:`resource use/watr/m3`, names_to = 'impact_category', values_to = 'value') %>%
  mutate(impact_category = gsub(' ', '_' , gsub('[[:punct:]]', '_', impact_category))) %>%
  filter(impact_category %in% c('impact_potential_gcc_kg_co2_eq', 'resource_use_land_m2_yr', 'resource_use_watr_m3',
                                'resource_use_enrg_mj', 'impact_potential_eutr_kg_n_eq')) 

# Domestic
impacts_outbound <- faf_impacts_long %>%
  filter(trade_type == 1, dms_orig != dms_dest) %>%
  group_by(dms_orig, impact_category) %>%
  summarize(value = sum(value)) %>%
  spread(impact_category, value)

impacts_inbound <- faf_impacts_long %>%
  filter(trade_type == 1, dms_orig != dms_dest) %>%
  group_by(dms_dest, impact_category) %>%
  summarize(value = sum(value)) %>%
  spread(impact_category, value) 

impacts_netdomestic <- bind_cols(region = impacts_outbound %>% ungroup %>% pull(dms_orig), as.data.frame(impacts_inbound[,-1] - impacts_outbound[,-1])) 
# Foreign
impacts_export <- faf_impacts_long %>%
  filter(trade_type == 3) %>%
  group_by(dms_orig, impact_category) %>%
  summarize(value = sum(value)) %>%
  spread(impact_category, value)

impacts_import <- faf_impacts_long %>%
  filter(trade_type == 2) %>%
  group_by(dms_dest, impact_category) %>%
  summarize(value = sum(value)) %>%
  spread(impact_category, value)

map_impact_outbound <- cfsmap %>% left_join(impacts_outbound, by = c('Code' = 'dms_orig'))
map_impact_inbound <- cfsmap %>% left_join(impacts_inbound, by = c('Code' = 'dms_dest'))
map_impact_net <- cfsmap %>% left_join(impacts_netdomestic, by = c('Code' = 'region'))


# Draw map ----------------------------------------------------------------

# Source fns from faf_bea_viz.r
draw_cfsmap_with_insets(map_impact_inbound %>% mutate(impact_potential_gcc_kg_co2_eq = impact_potential_gcc_kg_co2_eq/1e9), 
                        variable = impact_potential_gcc_kg_co2_eq, 
                        title = 'Inbound GHG impacts (domestic)',
                        scale_name = 'MT CO2 eq.')

draw_cfsmap_with_insets(map_impact_outbound %>% mutate(impact_potential_gcc_kg_co2_eq = impact_potential_gcc_kg_co2_eq/1e9), 
                        variable = impact_potential_gcc_kg_co2_eq, 
                        title = 'Outbound GHG impacts (domestic)',
                        scale_name = 'MT CO2 eq.')

draw_cfsmap_with_insets(map_impact_inbound %>% mutate(resource_use_land_m2_yr = resource_use_land_m2_yr/1e6), 
                        variable = resource_use_land_m2_yr, 
                        title = 'Inbound land impacts (domestic)',
                        scale_name = 'km2 land')

draw_cfsmap_with_insets(map_impact_outbound %>% mutate(resource_use_land_m2_yr = resource_use_land_m2_yr/1e9), 
                        variable = resource_use_land_m2_yr, 
                        title = 'Outbound land impacts (domestic)',
                        scale_name = 'km2 land')

draw_cfsmap_with_insets(map_impact_inbound %>% mutate(resource_use_land_m2_yr = resource_use_land_m2_yr/1e6), 
                        variable = resource_use_land_m2_yr, 
                        title = 'Inbound land impacts (domestic)',
                        scale_name = 'km2 land')

draw_cfsmap_with_insets(map_impact_outbound %>% mutate(resource_use_land_m2_yr = resource_use_land_m2_yr/1e6), 
                        variable = resource_use_land_m2_yr, 
                        title = 'Outbound land impacts (domestic)',
                        scale_name = 'km2 land')

draw_cfsmap_with_insets(map_impact_inbound %>% mutate(resource_use_watr_m3 = resource_use_watr_m3/1e9), 
                        variable = resource_use_watr_m3, 
                        title = 'Inbound water impacts (domestic)',
                        scale_name = 'km3 water')

draw_cfsmap_with_insets(map_impact_outbound %>% mutate(resource_use_watr_m3 = resource_use_watr_m3/1e9), 
                        variable = resource_use_watr_m3, 
                        title = 'Outbound water impacts (domestic)',
                        scale_name = 'km3 water')


# Draw net impacts --------------------------------------------------------



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
  size_ak <- 0.32
  size_hi <- 0.2
  three_maps <- ggdraw(us_map + add_theme) +
    draw_plot(ak_map, width = size_ak, height = size_ak * ratio_ak, x = 0.01, y = 0.15, vjust = 0) +
    draw_plot(hi_map, width = size_hi, height = size_hi * ratio_hi, x = 0.26, y = 0.15, vjust = 0)
  return(three_maps)
}

dark_theme <-  theme_void() +
  theme(panel.background = element_rect(fill = 'black', color = NA),
        plot.background = element_rect(color = "black", fill = "black"),
        legend.text = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        plot.title = element_text(color = 'white'),
        legend.position = 'bottom')

p_ghgmap <- draw_cfsmap_divergent(map_impact_net %>% mutate(impact_potential_gcc_kg_co2_eq = impact_potential_gcc_kg_co2_eq/1e9), 
                      variable = impact_potential_gcc_kg_co2_eq, 
                      title = 'Net domestic virtual GHG emissions transfers',
                      scale_name = parse(text = 'MT~CO[2]~eq.'),
                      scale_breaks = c(-50, -10, 0, 10, 50),
                      add_theme = dark_theme)

p_landmap <- draw_cfsmap_divergent(map_impact_net %>% mutate(resource_use_land_m2_yr = resource_use_land_m2_yr/1e6), 
                      variable = resource_use_land_m2_yr, 
                      title = 'Net domestic virtual land transfers',
                      scale_name = parse(text = 'km^2~land'),
                      scale_breaks = c(-250000, 0, 250000),
                      add_theme = dark_theme)

p_watermap <- draw_cfsmap_divergent(map_impact_net %>% mutate(resource_use_watr_m3 = resource_use_watr_m3/1e9), 
                      variable = resource_use_watr_m3, 
                      title = 'Net domestic virtual water transfers',
                      scale_name = parse(text = 'km^3~water'),
                      scale_breaks = c(-5, -2, 0, 2, 5),
                      add_theme = dark_theme)

ggsave(file.path(fp_out, 'maps/nettransfer_prelim_ghg.png'), p_ghgmap, height = 6, width = 9, dpi = 300)
ggsave(file.path(fp_out, 'maps/nettransfer_prelim_land.png'), p_landmap, height = 6, width = 9, dpi = 300)
ggsave(file.path(fp_out, 'maps/nettransfer_prelim_water.png'), p_watermap, height = 6, width = 9, dpi = 300)
