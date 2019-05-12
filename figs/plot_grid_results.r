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

x <- seq(0, 100, by = 10)
dat_ghg_reduced <- grid_result %>%
  mutate(L1 = L1*100, L2 = L2*100, L3 = L3*100) %>%
  filter(grepl('co2', impact_category), L1 %in% x, L2 %in% x, L3 %in% x, (L1 == 0 | L2 == 0 | L3 == 0)) %>%
  mutate(value = value/max(value))

ggplot(dat_ghg_reduced %>% filter(L3 == 0), aes(x = L1, y = L2, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0.9, 1)) +
  scale_x_continuous(expand = c(0,0), breaks = x, name = 'Reduction in production waste', labels = paste0(x, '%')) +
  scale_y_continuous(expand = c(0,0), breaks = x, name = 'Reduction in retail waste', labels = paste0(x, '%')) +
  ggtitle('GHG reduction by FLW reduction', 'reduction in production and retail waste, holding consumer waste constant') +
  theme(legend.position = 'none')

ggplot(dat_ghg_reduced %>% filter(L2 == 0), aes(x = L1, y = L3, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0.9, 1)) +
  scale_x_continuous(expand = c(0,0), breaks = x, name = 'Reduction in production waste', labels = paste0(x, '%')) +
  scale_y_continuous(expand = c(0,0), breaks = x, name = 'Reduction in consumer waste', labels = paste0(x, '%')) +
  ggtitle('GHG reduction by FLW reduction', 'reduction in production and consumer waste, holding retail waste constant') +
  theme(legend.position = 'none')

ggplot(dat_ghg_reduced %>% filter(L1 == 0), aes(x = L2, y = L3, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0.9, 1), name = 'Impact relative\n to baseline', breaks = c(.9, .95, 1), labels = c('90%', '95%', '100%')) +
  scale_x_continuous(expand = c(0,0), breaks = x, name = 'Reduction in retail waste', labels = paste0(x, '%')) +
  scale_y_continuous(expand = c(0,0), breaks = x, name = 'Reduction in consumer waste', labels = paste0(x, '%')) +
  ggtitle('GHG reduction by FLW reduction', 'reduction in retail and consumer waste, holding production waste constant') 
