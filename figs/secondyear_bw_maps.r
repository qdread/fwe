# Dark background maps of beer and cheese transfers for displaying as preliminary spatial results for the talk on Dec 3 2019

draw_cfsmap_with_insets <- function(map_data, variable, title, scale_name = 'Value\n(billion $)', add_theme = theme_void()) {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # Calculate scale range
  variable <- enquo(variable)
  map_data <- map_data %>% mutate(!!variable := !!variable/1000)
  scale_range <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  
  # Draw the three maps
  us_map <- ggplot(map_data %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    scale_fill_viridis_c(na.value = 'gray75', name = scale_name, limits = scale_range, guide = guide_colorbar(direction = 'horizontal')) +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
  # Include insets for Alaska and Hawaii
  # Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
  hi_map <- ggplot(map_data %>% filter(grepl('Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
    scale_fill_viridis_c(na.value = 'gray75', limits = scale_range) +
    add_theme + 
    theme(legend.position = 'none')
  
  ak_map <- ggplot(map_data %>% filter(grepl('Alaska', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
    scale_fill_viridis_c(na.value = 'gray75', limits = scale_range) +
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

# Define function to make map for all four variables for a single code
# --------------------------------------------------------------------

draw_four_maps <- function(variable, main_title, addtheme = theme()) {
  map_list <- map2(list(cfsmap_inbound, cfsmap_outbound, cfsmap_imports, cfsmap_exports), c('Domestic inbound', 'Domestic outbound', 'Foreign inbound', 'Foreign outbound'),
                   ~ draw_cfsmap_with_insets(map_data = .x, variable = !!enquo(variable), title = .y, add_theme = addtheme))
  map_grid <- plot_grid(plotlist = map_list, nrow = 2)
  
  # Add title as described in cowplot vignette
  title_obj <- ggdraw() + 
    draw_label(main_title, fontface = 'bold', color = 'white', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7)) +
    addtheme
  
  map_grid <- plot_grid(title_obj + addtheme, map_grid, ncol = 1, rel_heights = c(0.1, 1))
  
  return(map_grid)
}

food_industries <- concordance %>% 
  filter(SCTG.Code %in% 1:8) %>%
  mutate(shortname = substr(BEA_def, 1, 20) %>% trimws %>% tolower,
         shortname = gsub('[[:punct:]]', '', shortname),
         shortname = gsub(' ', '_', shortname))

idx <- c(2, 28, 36, 21)

dark_theme <-  theme_void() +
  theme(panel.background = element_rect(fill = 'black', color = NA),
        plot.background = element_rect(color = "black", fill = "black"),
        legend.text = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        plot.title = element_text(color = 'white'),
        legend.position = 'bottom')

maps1 <- draw_four_maps(`1111B0`, "Grain farming", dark_theme)
maps2 <- draw_four_maps(`311810`, "Bread and bakery product manufacturing", dark_theme)
maps3 <- draw_four_maps(`312120`, "Breweries", dark_theme)
maps4 <- draw_four_maps(`311513`, "Cheese manufacturing", dark_theme)

ggsave(file.path(fp_out, 'maps', 'darkmap_grainfarming.png'), maps1 + theme(plot.background = element_rect(fill = 'black')), height = 8, width = 10, dpi = 300)
ggsave(file.path(fp_out, 'maps', 'darkmap_bread.png'), maps2 + theme(plot.background = element_rect(fill = 'black')), height = 8, width = 10, dpi = 300)
ggsave(file.path(fp_out, 'maps', 'darkmap_beer.png'), maps3 + theme(plot.background = element_rect(fill = 'black')), height = 8, width = 10, dpi = 300)
ggsave(file.path(fp_out, 'maps', 'darkmap_cheese.png'), maps4 + theme(plot.background = element_rect(fill = 'black')), height = 8, width = 10, dpi = 300)

