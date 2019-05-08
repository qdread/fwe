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
