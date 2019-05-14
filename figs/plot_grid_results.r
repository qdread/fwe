# Create plots of scenario results including full grid of reductions
# QDR / FWE / 07 May 2019

library(tidyverse)

fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
fp_fig <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'figures')
grid_result <- read.csv(file.path(fp_output, 'fao_grid_scenario_lcia_results.csv'), stringsAsFactors = FALSE)

# Figure showing benefits per percentage reduction for the three different stages, done in isolation

grid_result_individual <- grid_result %>%
  filter(L1+L2 == 0 | L1+L3 == 0 | L2+L3 == 0) %>%
  mutate(stage_reduced = case_when(L1 > 0 ~ 'production', L2 > 0 ~ 'retail', L3 > 0 ~ 'consumer'),
         proportion_reduced = L1+L2+L3)

grid_result_individual %>%
  filter(grepl('co2', impact_category)) %>%
  mutate(value = value/max(value)) %>%
  filter(!is.na(stage_reduced)) %>%
  ggplot(aes(x = proportion_reduced, y = value, group = stage_reduced, color = stage_reduced)) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(limits = c(0.8, 1), name = 'GHG emissions relative to baseline')

# Try to make a 3d surface plot

library(plotly)
Sys.setenv('plotly_username' = 'qdread')
Sys.setenv('plotly_api_key' = '6Pp0DLQnB1ZlCcbh0V5s')

reduced_seq <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
p3d_ghg <- grid_result %>%
  filter(grepl('co2', impact_category), L1 %in% reduced_seq, L2 %in% reduced_seq, L3 %in% reduced_seq) %>%
  mutate(value = value/max(value)) %>%
  plot_ly(x = ~ L1, 
          y = ~ L2, 
          z = ~ L3, 
          marker = list(color = ~ value, showscale = TRUE, colorscale = 'Viridis'), 
          text = ~ paste('Impact (baseline=1):', round(value, 3))) %>%
  add_markers() %>%
  layout(title = 'GHG reduction by FLW reduction',
         scene = list(xaxis = list(title = 'Waste reduction in L1 (production)', tickformat = '%'),
                      yaxis = list(title = 'Waste reduction in L2 (distribution)', tickformat = '%'),
                      zaxis = list(title = 'Waste reduction in L3 (consumption)', tickformat = '%')))

p3d_land <- grid_result %>%
  filter(grepl('land', impact_category), L1 %in% reduced_seq, L2 %in% reduced_seq, L3 %in% reduced_seq) %>%
  mutate(value = value/max(value)) %>%
  plot_ly(x = ~ L1, 
          y = ~ L2, 
          z = ~ L3, 
          marker = list(color = ~ value, showscale = TRUE, colorscale = 'Viridis'), 
          text = ~ paste('Impact (baseline=1):', round(value, 3))) %>%
  add_markers() %>%
  layout(title = 'Land use reduction by FLW reduction',
         scene = list(xaxis = list(title = 'Waste reduction in L1 (production)', tickformat = '%'),
                      yaxis = list(title = 'Waste reduction in L2 (distribution)', tickformat = '%'),
                      zaxis = list(title = 'Waste reduction in L3 (consumption)', tickformat = '%')))

p3d_watr <- grid_result %>%
  filter(grepl('watr', impact_category), L1 %in% reduced_seq, L2 %in% reduced_seq, L3 %in% reduced_seq) %>%
  mutate(value = value/max(value)) %>%
  plot_ly(x = ~ L1, 
          y = ~ L2, 
          z = ~ L3, 
          marker = list(color = ~ value, showscale = TRUE, colorscale = 'Viridis'), 
          text = ~ paste('Impact (baseline=1):', round(value, 3))) %>%
  add_markers() %>%
  layout(title = 'Water use reduction by FLW reduction',
         scene = list(xaxis = list(title = 'Waste reduction in L1 (production)', tickformat = '%'),
                      yaxis = list(title = 'Waste reduction in L2 (distribution)', tickformat = '%'),
                      zaxis = list(title = 'Waste reduction in L3 (consumption)', tickformat = '%')))

p3d_enrg <- grid_result %>%
  filter(grepl('enrg', impact_category), L1 %in% reduced_seq, L2 %in% reduced_seq, L3 %in% reduced_seq) %>%
  mutate(value = value/max(value)) %>%
  plot_ly(x = ~ L1, 
          y = ~ L2, 
          z = ~ L3, 
          marker = list(color = ~ value, showscale = TRUE, colorscale = 'Viridis'), 
          text = ~ paste('Impact (baseline=1):', round(value, 3))) %>%
  add_markers() %>%
  layout(title = 'Energy use reduction by FLW reduction',
         scene = list(xaxis = list(title = 'Waste reduction in L1 (production)', tickformat = '%'),
                      yaxis = list(title = 'Waste reduction in L2 (distribution)', tickformat = '%'),
                      zaxis = list(title = 'Waste reduction in L3 (consumption)', tickformat = '%')))

api_create(p3d_ghg, 'ghg_reduction_3dplot')
api_create(p3d_land, 'landuse_reduction_3dplot')
api_create(p3d_watr, 'wateruse_reduction_3dplot')
api_create(p3d_enrg, 'energyuse_reduction_3dplot')


# 2d plots of two reduction combos ----------------------------------------

library(cowplot)

# Function to produce grid plot for a given impact category.

threegrids <- function(cat_name, main_title, color_limits = c(0.9, 1), color_breaks = c(0.9, 0.95, 1)) {
  x <- seq(0, 100, by = 10)
  dat_reduced <- grid_result %>%
    mutate(L1 = L1*100, L2 = L2*100, L3 = L3*100) %>%
    filter(grepl(cat_name, impact_category), L1 %in% x, L2 %in% x, L3 %in% x, (L1 == 0 | L2 == 0 | L3 == 0)) %>%
    mutate(value = value/max(value))
  color_labels <- paste0(color_breaks * 100, '%')
  
  p12 <- ggplot(dat_reduced %>% filter(L3 == 0), aes(x = L1, y = L2, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(limits = color_limits) +
    scale_x_continuous(expand = c(0,0), breaks = x, name = 'Reduction in production waste', labels = paste0(x, '%')) +
    scale_y_continuous(expand = c(0,0), breaks = x, name = 'Reduction in retail waste', labels = paste0(x, '%')) +
    ggtitle('Reduction in production and retail waste,\nholding consumer waste constant') +
    theme(legend.position = 'none', plot.title = element_text(size = 10, face = 'plain'), axis.text = element_text(size = 10)) +
    panel_border(colour = 'black')
  
  p13 <- ggplot(dat_reduced %>% filter(L2 == 0), aes(x = L1, y = L3, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(limits = color_limits) +
    scale_x_continuous(expand = c(0,0), breaks = x, name = 'Reduction in production waste', labels = paste0(x, '%')) +
    scale_y_continuous(expand = c(0,0), breaks = x, name = 'Reduction in consumer waste', labels = paste0(x, '%')) +
    ggtitle('Reduction in production and consumer waste,\nholding retail waste constant') +
    theme(legend.position = 'none', plot.title = element_text(size = 10, face = 'plain'), axis.text = element_text(size = 10)) +
    panel_border(colour = 'black')
  
  p23 <- ggplot(dat_reduced %>% filter(L1 == 0), aes(x = L2, y = L3, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(limits = color_limits, name = 'Impact relative\n to baseline', breaks = color_breaks, labels = color_labels) +
    scale_x_continuous(expand = c(0,0), breaks = x, name = 'Reduction in retail waste', labels = paste0(x, '%')) +
    scale_y_continuous(expand = c(0,0), breaks = x, name = 'Reduction in consumer waste', labels = paste0(x, '%')) +
    ggtitle('Reduction in retail and consumer waste,\nholding production waste constant') +
    theme(plot.title = element_text(size = 10, face = 'plain'), axis.text = element_text(size = 10)) +
    panel_border(colour = 'black')
  
  joint_title <- ggdraw() + draw_label(main_title, fontface='bold')
  
  p_row <- plot_grid(plotlist = list(p12, p13, p23), nrow = 1, rel_widths = c(1, 1, 1.3)) 
  
  p_final <- plot_grid(joint_title, p_row, ncol=1, rel_heights=c(0.1, 1))
  
  return(p_final)
  
}

# Print minima, where at least one category is zero
grid_result %>% 
  filter(L1 == 0 | L2 == 0 | L3 == 0) %>%
  group_by(impact_category) %>% 
  summarize(min_relative=min(value)/max(value)) %>% 
  print(n=nrow(.))

lims <- c(0.87, 1)
brks <- c(0.9, 0.95, 1)
p_ghg <- threegrids('co2', 'GHG reduction with FLW reduction', lims, brks)
p_land <- threegrids('land', 'Land use reduction with FLW reduction', lims, brks)
p_enrg <- threegrids('enrg', 'Energy use reduction with FLW reduction', lims, brks)
p_watr <- threegrids('watr', 'Water use reduction with FLW reduction', lims, brks) 

# Draw pngs of the results
ggsave(file.path(fp_fig, 'reduction_grid_GHG.png'), p_ghg, height = 5, width = 15, dpi = 300)
ggsave(file.path(fp_fig, 'reduction_grid_land.png'), p_land, height = 5, width = 15, dpi = 300)
ggsave(file.path(fp_fig, 'reduction_grid_energy.png'), p_enrg, height = 5, width = 15, dpi = 300)
ggsave(file.path(fp_fig, 'reduction_grid_water.png'), p_watr, height = 5, width = 15, dpi = 300)