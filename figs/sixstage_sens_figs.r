# Figures showing six stage grid reduction results, with information from sensitivity analysis included
# QDR / FWE / 27 June 2019

library(tidyverse)
fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')

stage_full_names <- c('production', 'processing', 'retail', 'consumption: food service', 'consumption: institutional', 'consumption: household')

optimal_df_all <- read.csv(file.path(fp_output, 'sixstage_scenario_opt_results.csv'))
grid_result <- read.csv(file.path(fp_output, 'sixstage_scenario_grid_lcia_results.csv'), stringsAsFactors = FALSE)
grid_sensitivity_df <- read.csv(file.path(fp_output, 'sensitivity_grid_alldraws.csv'), stringsAsFactors = FALSE)
grid_sensitivity_CIs <- read.csv(file.path(fp_output, 'sensitivity_grid_CIs.csv'), stringsAsFactors = FALSE)

stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfood service', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')

# Calculate sequences for each draw ---------------------------------------

create_sequence <- function(dat, value_id, proportion) {
  value_id <- enquo(value_id)
  dat %>%
    ungroup %>%
    mutate(nbypct = rowSums(.[,1:6] == proportion)) %>%
    filter(rowSums(.[,1:6] > 0) == nbypct) %>%
    group_by(nbypct) %>%
    filter(!!value_id == min(!!value_id)) %>%
    arrange(nbypct) %>%
    ungroup %>%
    mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))
}

# Do all sequences for each parameter draw.

# All sequences, all categories
allseq_allcats <- grid_sensitivity_df %>%
  filter(grepl('co2|watr|land|enrg|eutr', impact_category)) %>%
  group_by(draw_id, impact_category) %>%
  do(create_sequence(., value, 0.5))

# Do the sequence for the "real" values of the parameters.
trueseq <- grid_result %>%
  filter(grepl('co2|watr|land|enrg|eutr', impact_category)) %>%
  group_by(impact_category) %>%
  do(create_sequence(., value, 0.5))

# Match the true sequence values with the error bar values.
trueseq_withcis <- trueseq %>% left_join(grid_sensitivity_CIs)

# Function to create reduction plot with error bars
reduction_plot_with_errorbars <- function(sequence, yval, yminval, ymaxval, plot_title, plot_subtitle) {
  yval <- enquo(yval)
  yminval <- enquo(yminval)
  ymaxval <- enquo(ymaxval)
  sequence %>%
    mutate(norm = max(!!yval)) %>%
    mutate(!!yval := !!yval/norm, 
           !!yminval := !!yminval/norm,
           !!ymaxval := !!ymaxval/norm) %>%
    mutate(stage_reduced = stage_full_names_lookup[stage_reduced]) %>%
    ggplot(aes(x = nbypct, y = !!yval, ymin = !!yminval, ymax = !!ymaxval)) +
    geom_line(size = 1) +
    geom_errorbar(width = 0.2, color = 'gray60') +
    geom_point(size = 3, color = 'white', fill = 'black', shape = 21, stroke = 2) +
    geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25) +
    scale_x_continuous(name = 'Number of sectors where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent, limits = c(0.75, 1.03), expand = c(0,0)) +
    ggtitle(plot_title, plot_subtitle) +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.subtitle = element_text(size = 10))
  
}

subtitle <- 'done by sequentially reducing waste in the FSC stage \nwith greatest impact by 50%'

# Draw the plots for each impact category
co2_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('co2', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975, 'Impact of FLW reduction on GHG emissions', subtitle)
land_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('land', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975, 'Impact of FLW reduction on land use', subtitle)
water_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('watr', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975, 'Impact of FLW reduction on water use', subtitle)
energy_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('enrg', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975, 'Impact of FLW reduction on energy use', subtitle) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent, limits = c(0.75, 1.05), expand = c(0,0))
eutr_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('eutr', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975, 'Impact of FLW reduction on eutrophication', subtitle)

# Save the plots
ggsave(file.path(fpfig, 'sixstage_gridwithci_co2by50pct.png'), co2_seqplot_ci, height = 6, width = 6, dpi = 300)
ggsave(file.path(fpfig, 'sixstage_gridwithci_landby50pct.png'), land_seqplot_ci, height = 6, width = 6, dpi = 300)
ggsave(file.path(fpfig, 'sixstage_gridwithci_waterby50pct.png'), water_seqplot_ci, height = 6, width = 6, dpi = 300)
ggsave(file.path(fpfig, 'sixstage_gridwithci_energyby50pct.png'), energy_seqplot_ci, height = 6, width = 6, dpi = 300)
ggsave(file.path(fpfig, 'sixstage_gridwithci_eutrby50pct.png'), eutr_seqplot_ci, height = 6, width = 6, dpi = 300)

# Create a table showing mean rank or number of times things swap orders
allseq_allcats %>%
  group_by(impact_category, nbypct, stage_reduced) %>%
  summarize(n = n()) %>%
  spread(stage_reduced, n, fill = 0)
  
meanrank_allcats <- allseq_allcats %>%
  group_by(impact_category, stage_reduced) %>%
  summarize(mean_rank = mean(nbypct),
            proportion_not_swapped = sum(nbypct == round(mean_rank))) %>%
  arrange(impact_category, mean_rank) %>%
  filter(!stage_reduced %in% 'none') %>%
  ungroup %>%
  mutate(stage_reduced = setNames(stage_full_names, c('L1','L2','L3','L4a','L4b','L5'))[stage_reduced],
         impact_category = rep(c('eutrophication', 'GHG emissions', 'energy use', 'land use', 'water use'), each = 6))
write.csv(meanrank_allcats, file.path(fp_output, 'sensitivity_grid_summarytable.csv'), row.names = FALSE)

# Create flex table.
library(flextable)
meanrank_ft <- meanrank_allcats %>%
  mutate(proportion_not_swapped = paste0(proportion_not_swapped, '%')) %>%
  flextable %>%
  theme_vanilla %>%
  merge_v(j = 'impact_category') %>%
  set_header_labels(impact_category = 'category', stage_reduced = 'stage', mean_rank = 'mean rank', proportion_not_swapped = 'percent not swapped')

# Do the 100% sequences and create plot -----------------------------------

allseq100_allcats <- grid_sensitivity_df %>%
  filter(grepl('co2|watr|land|enrg|eutr', impact_category)) %>%
  group_by(draw_id, impact_category) %>%
  do(create_sequence(., value, 1.0))

meanrank100_allcats <- allseq100_allcats %>%
  group_by(impact_category, stage_reduced) %>%
  summarize(mean_rank = mean(nbypct))

# Do the sequence for the "real" values of the parameters.
trueseq100 <- grid_result %>%
  filter(grepl('co2|watr|land|enrg|eutr', impact_category)) %>%
  group_by(impact_category) %>%
  do(create_sequence(., value, 1.0))

# Match the true sequence values with the error bar values.
trueseq100_withcis <- trueseq100 %>% left_join(grid_sensitivity_CIs)

# Create single plot
# Need to include dodge because of the overlapping error bars
pd <- position_dodge(width = 0.15)
allseq100plot <- trueseq100_withcis %>%
  mutate(stage_reduced = stage_full_names_lookup[stage_reduced]) %>%
  group_by(impact_category) %>%
  mutate(norm = max(value)) %>%
  mutate(value = value/norm, q025 = q025/norm, q975 = q975/norm) %>%
  ungroup %>%
  mutate(impact_category = rep(c('eutrophication', 'GHG', 'water', 'land', 'energy'), each = 7)) %>%
  ggplot(aes(x = nbypct, y = value, ymin = q025, ymax = q975, group = impact_category)) +
  geom_line(aes(color = impact_category), size = 1, position = pd) +
  geom_errorbar(width = 0.2, color = 'gray60', position = pd) +
  geom_point(aes(color = impact_category), size = 3, position = pd) +
  scale_x_continuous(name = 'Number of sectors where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent, limits = c(0.75, 1.05)) +
  scale_color_brewer(name = 'Impact category', type = 'qual', palette = 'Set2') +
  ggtitle('Impact of FLW reduction on environmental metrics', 'done by sequentially reducing waste in the FSC stage \nwith greatest impact by 100%') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(size = 10),
        legend.position = c(0.8, 0.8)) 

ggsave(file.path(fpfig, 'sixstage_gridwithci_allcategoriesby100pct.png'), allseq100plot, height = 6, width = 6, dpi = 300)


# Optimization sensitivity errorbars --------------------------------------

# Parameters
optimal_df_all <- read.csv(file.path(fp_output, 'sixstage_scenario_opt_results.csv'), stringsAsFactors = FALSE)
optim_pars_sens <- read.csv(file.path(fp_output, 'opt_sensitivity_pars.csv'), stringsAsFactors = FALSE)

optim_pars_sens_ci <- optim_pars_sens %>%
  group_by(category, total_cost, stage) %>%
  summarize(cost_q025 = round(quantile(cost, 0.025)), cost_q975 = round(quantile(cost, 0.975)))

optim_pars_withci <- left_join(optimal_df_all, optim_pars_sens_ci)
optim_pars_withci %>% group_by(total_cost) %>% summarize(n = sum(cost > cost_q975 | cost < cost_q025))

pd <- position_dodge(width = 50)

ggplot(optim_pars_withci, aes(x = total_cost, y = cost, ymin = cost_q025, ymax = cost_q975, color = stage, group = stage)) +
  facet_wrap(~ category) +
  geom_errorbar(color = 'gray50', width = 100, position = pd) +
  geom_point(size = 2, position = pd) + 
  geom_line(position = pd) +
  scale_x_continuous(name = 'Total invested in FLW reduction (million USD)', breaks = c(0, 500, 1000, 2000)) +
  scale_y_continuous(name = 'Amount invested in each stage (million USD)') +
  scale_color_brewer(type='qual', palette='Set2') +
  ggtitle('Optimal allocation of FLW reduction funds to minimize environmental impact', 'for a number of possible total investments') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        legend.position = 'bottom')


# Values
# Compare optimization results to the baseline.
optimal_value_all <- read.csv(file.path(fp_output, 'sixstage_scenario_opt_values.csv'), stringsAsFactors = FALSE)

# Add baseline values.
baseline <- read.csv(file.path(fp_output, 'sixstage_scenario_grid_lcia_results.csv'), stringsAsFactors = FALSE) %>%
  filter(rowSums(.[,1:6]) == 0)

baseline <- baseline %>% 
  filter(grepl('gcc|land|watr|enrg|eutr', impact_category)) %>%
  mutate(category = c('energy', 'eutrophication', 'GHG', 'land', 'water'), total_cost = 0)

optimal_value_all <- optimal_value_all %>%
  rbind(baseline[,c('category','total_cost','value')]) %>%
  group_by(category) %>%
  mutate(value = value/max(value))

ggplot(optimal_value_all, aes(x = total_cost, y = value, color = category)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks = c(0,500,1000,2000,5000), name = 'Total invested (million $)') +
  scale_y_continuous(labels = scales::percent, name = 'Impact (percent of baseline)') +
  scale_color_brewer(name = 'Impact category', type = 'qual', palette = 'Set2') +
  ggtitle('Impact reduction by total invested', 'optimizing for each impact category separately') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.8, 0.8))