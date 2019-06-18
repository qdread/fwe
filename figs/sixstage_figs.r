# Figures for six-stage scenario results
# QDR / FWE / 31 May 2019

library(tidyverse)
fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')

stage_full_names <- c('production', 'processing', 'retail', 'consumption: food service', 'consumption: institutional', 'consumption: household')
optimal_df_all <- read.csv(file.path(fp_output, 'sixstage_scenario_opt_results.csv'))
grid_result <- read.csv(file.path(fp_output, 'sixstage_scenario_grid_lcia_results.csv'), stringsAsFactors = FALSE)

# Grid plots
# ==========

# This might be more difficult to plot because of the 6 different stages where reduction occurs.
# They are more or less independent so we can show the reductions in impacts when each one is reduced sequentially.

# Sequential impact reduction of 50% waste reduction in each sector.
co2sequence <- grid_result %>%
  filter(grepl('co2', impact_category)) %>%
  mutate(nbypct = rowSums(.[,1:6] == 0.5)) %>%
  filter(rowSums(.[,1:6] > 0) == nbypct) %>%
  group_by(nbypct) %>%
  filter(value == min(value)) %>%
  arrange(nbypct) %>%
  ungroup %>%
  mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))

watrsequence <- grid_result %>%
  filter(grepl('watr', impact_category)) %>%
  mutate(nbypct = rowSums(.[,1:6] == 0.5)) %>%
  filter(rowSums(.[,1:6] > 0) == nbypct) %>%
  group_by(nbypct) %>%
  filter(value == min(value)) %>%
  arrange(nbypct) %>%
  ungroup %>%
  mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))

landsequence <- grid_result %>%
  filter(grepl('land', impact_category)) %>%
  mutate(nbypct = rowSums(.[,1:6] == 0.5)) %>%
  filter(rowSums(.[,1:6] > 0) == nbypct) %>%
  group_by(nbypct) %>%
  filter(value == min(value)) %>%
  arrange(nbypct) %>%
  ungroup %>%
  mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))

enrgsequence <- grid_result %>%
  filter(grepl('enrg', impact_category)) %>%
  mutate(nbypct = rowSums(.[,1:6] == 0.5)) %>%
  filter(rowSums(.[,1:6] > 0) == nbypct) %>%
  group_by(nbypct) %>%
  filter(value == min(value)) %>%
  arrange(nbypct) %>%
  ungroup %>%
  mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))

stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfood service', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')

co2seqplot <- co2sequence %>%
  mutate(value = value/max(value),
         stage_reduced = stage_full_names_lookup[stage_reduced]) %>%
  ggplot(aes(x = nbypct, y = value)) +
    geom_line(size = 1) +
    geom_point(size = 3, color = 'white', fill = 'black', shape = 21, stroke = 2) +
    geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25) +
    scale_x_continuous(name = 'Number of sectors where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent, limits = c(0.75, 1)) +
    ggtitle('Impact of FLW reduction on GHG emissions', 'done by sequentially reducing waste in the FSC stage \nwith greatest impact by 50%') +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.subtitle = element_text(size = 10)) 

landseqplot <- landsequence %>%
  mutate(value = value/max(value),
         stage_reduced = stage_full_names_lookup[stage_reduced]) %>%
  ggplot(aes(x = nbypct, y = value)) +
  geom_line(size = 1) +
  geom_point(size = 3, color = 'white', fill = 'black', shape = 21, stroke = 2) +
  geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25) +
  scale_x_continuous(name = 'Number of sectors where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent, limits = c(0.75, 1)) +
  ggtitle('Impact of FLW reduction on land use', 'done by sequentially reducing waste in the FSC stage \nwith greatest impact by 50%') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(size = 10)) 

watrseqplot <- watrsequence %>%
  mutate(value = value/max(value),
         stage_reduced = stage_full_names_lookup[stage_reduced]) %>%
  ggplot(aes(x = nbypct, y = value)) +
  geom_line(size = 1) +
  geom_point(size = 3, color = 'white', fill = 'black', shape = 21, stroke = 2) +
  geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25) +
  scale_x_continuous(name = 'Number of sectors where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent, limits = c(0.75, 1)) +
  ggtitle('Impact of FLW reduction on water use', 'done by sequentially reducing waste in the FSC stage \nwith greatest impact by 50%') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(size = 10)) 

enrgseqplot <- enrgsequence %>%
  mutate(value = value/max(value),
         stage_reduced = stage_full_names_lookup[stage_reduced]) %>%
  ggplot(aes(x = nbypct, y = value)) +
  geom_line(size = 1) +
  geom_point(size = 3, color = 'white', fill = 'black', shape = 21, stroke = 2) +
  geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25) +
  scale_x_continuous(name = 'Number of sectors where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent, limits = c(0.75, 1)) +
  ggtitle('Impact of FLW reduction on energy use', 'done by sequentially reducing waste in the FSC stage \nwith greatest impact by 50%') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(size = 10)) 

# Also do by 100% just to see.
co2sequence100 <- grid_result %>%
  filter(grepl('co2', impact_category)) %>%
  mutate(nbypct = rowSums(.[,1:6] == 1)) %>%
  filter(rowSums(.[,1:6] > 0) == nbypct) %>%
  group_by(nbypct) %>%
  filter(value == min(value)) %>%
  arrange(nbypct) %>%
  ungroup %>%
  mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))

watrsequence100 <- grid_result %>%
  filter(grepl('watr', impact_category)) %>%
  mutate(nbypct = rowSums(.[,1:6] == 1)) %>%
  filter(rowSums(.[,1:6] > 0) == nbypct) %>%
  group_by(nbypct) %>%
  filter(value == min(value)) %>%
  arrange(nbypct) %>%
  ungroup %>%
  mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))

landsequence100 <- grid_result %>%
  filter(grepl('land', impact_category)) %>%
  mutate(nbypct = rowSums(.[,1:6] == 1)) %>%
  filter(rowSums(.[,1:6] > 0) == nbypct) %>%
  group_by(nbypct) %>%
  filter(value == min(value)) %>%
  arrange(nbypct) %>%
  ungroup %>%
  mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))

enrgsequence100 <- grid_result %>%
  filter(grepl('enrg', impact_category)) %>%
  mutate(nbypct = rowSums(.[,1:6] == 1)) %>%
  filter(rowSums(.[,1:6] > 0) == nbypct) %>%
  group_by(nbypct) %>%
  filter(value == min(value)) %>%
  arrange(nbypct) %>%
  ungroup %>%
  mutate(stage_reduced = c('none', names(sort(apply(.[,1:6],2,function(x) which(diff(x) > 0))))))

# Show all 100 plots on the same plot.
allseq100plot <- rbind(co2sequence100, watrsequence100, landsequence100, enrgsequence100) %>%
  mutate(stage_reduced = stage_full_names_lookup[stage_reduced]) %>%
  group_by(impact_category) %>%
  mutate(value = value / max(value)) %>%
  ungroup %>%
  mutate(impact_category = rep(c('GHG', 'water', 'land', 'energy'), each = 7)) %>%
  ggplot(aes(x = nbypct, y = value, group = impact_category)) +
    geom_line(aes(color = impact_category), size = 1) +
    geom_point(aes(color = impact_category), size = 3) +
    #geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25) +
    scale_x_continuous(name = 'Number of sectors where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent, limits = c(0.75, 1)) +
    scale_color_brewer(name = 'Impact category', type = 'qual', palette = 'Set2') +
    ggtitle('Impact of FLW reduction on environmental metrics', 'done by sequentially reducing waste in the FSC stage \nwith greatest impact by 100%') +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.subtitle = element_text(size = 10),
          legend.position = c(0.8, 0.8)) 

fpfig <- file.path(fp, 'figures')    
ggsave(file.path(fpfig, 'sixstage_grid_co2by50pct.png'), co2seqplot, height = 6, width = 6, dpi = 300)
ggsave(file.path(fpfig, 'sixstage_grid_landby50pct.png'), landseqplot, height = 6, width = 6, dpi = 300)
ggsave(file.path(fpfig, 'sixstage_grid_waterby50pct.png'), watrseqplot, height = 6, width = 6, dpi = 300)
ggsave(file.path(fpfig, 'sixstage_grid_energyby50pct.png'), enrgseqplot, height = 6, width = 6, dpi = 300)
ggsave(file.path(fpfig, 'sixstage_grid_allcategoriesby100pct.png'), allseq100plot, height = 6, width = 6, dpi = 300)

# Radar charts showing tradeoffs among scenarios
# ==============================================

# Radar plot to visualize several relevant categories at once
library(ggradar)

# the radar plot requires "wide" data
# We want this for the ones where a single sector is reduced by 50%.

category_lookup <- data.frame(impact_category = c("impact potential/eutr/kg n eq", "impact potential/gcc/kg co2 eq", 
                                "resource use/enrg/mj", "resource use/land/m2*yr", "resource use/watr/m3"),
                              impact_category_name = c('eutrophication\npotential', 'GHG emissions', 'energy use', 'land use', 'water use'))

radardat_singlestagereduced <- grid_result %>%
  filter((rowSums(.[,1:6]) == 0.5 & rowSums(.[,1:6] > 0) == 1) | (rowSums(.[,1:6]) == 0), grepl('enrg|watr|land|co2|eutr', impact_category)) %>%
  group_by(impact_category) %>%
  mutate(value = value / max(value)) %>%
  ungroup %>%
  filter(value < 1) %>%
  mutate(scenario = factor(stage_full_names[apply(.[,1:6], 1, function(x) which(x>0))], levels = stage_full_names),
         value = 1 - value) %>%
  left_join(category_lookup) %>%
  select(scenario, impact_category_name, value) %>%
  spread(impact_category_name, value)

radar1 <- ggradar(radardat_singlestagereduced, 
        grid.max = 0.05, label.gridline.max = TRUE, label.gridline.min = TRUE, axis.label.size = 3.5, values.radar = c('0%', '2.5%', '5%')) +
  ggtitle('Magnitude of impact reduction by waste reduction scenario', '50% waste reduction at single supply chain stages') +
  theme(title = element_text(size = 12), legend.position = 'bottom')

ggsave(file.path(fpfig, 'sixstage_radar_singlestages.png'), radar1, height = 12, width = 8, dpi = 300)

# Fake cost curves for optimization
# =================================

# Plot the different curves. (added 13 May)
# Generate the data.
params_for_curves <- data.frame(W0 = baseline_waste_rate, Wu = fake_unavoidable_waste_rate, B = B_sectors, nu = nu_sectors)
yvals_curves <- pmap(params_for_curves, waste_rate_by_cost, x = seq(0, 1000, 1))
dat_for_curves <- imap_dfr(yvals_curves, ~ data.frame(sector_name = naics_foodsystem$BEA_389_def[.y], stage = naics_foodsystem$stage_code[.y], params_for_curves[.y, ], x = seq(0, 1000, 1), y = .x))

ggplot(dat_for_curves %>% filter(stage %in% c('L1','L2','L3','L4a')), aes(x = x, y = y, color = stage, group = sector_name)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(type='qual', palette='Set2', labels = c('production', 'processing', 'retail', 'consumption')) +
  scale_x_continuous(name = 'Cost (relative units)', expand = c(0,0)) +
  scale_y_continuous(name = 'Waste rate', expand = c(0,0), labels = scales::percent, limits = c(0, 0.26)) +
  ggtitle('\"Fake\" cost curves for each FSC sector', 'Each stage has multiple sectors')
ggsave('/nfs/qread-data/figures/sixstage_costcurve_fake_example.png', height = 6, width = 6, dpi = 300)


# Results of optimization
# =======================

# Plot all optima.

optimal_df_all <- optimal_df_all %>%
  mutate(stage = factor(stage, levels = stage_full_names))

ggplot(optimal_df_all, aes(x = total_cost, y = cost, color = stage, group = stage)) +
  facet_wrap(~ category) +
  geom_point(size = 2) + geom_line() +
  scale_x_continuous(name = 'Total invested in FLW reduction (million USD)', breaks = c(0, 500, 1000, 2000, 5000)) +
  scale_y_continuous(name = 'Amount invested in each stage (million USD)') +
  scale_color_brewer(type='qual', palette='Set2') +
  ggtitle('Optimal allocation of FLW reduction funds to minimize environmental impact', 'for a number of possible total investments') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        legend.position = 'bottom')

ggsave(file.path(fpfig, 'sixstage_costcurve_allocations4impacts_lineplot.png'), height = 9, width = 9, dpi = 300)

# Do with bars.
ggplot(optimal_df_all, aes(x = total_cost, y = cost, fill = stage, group = stage)) +
  facet_wrap(~ category) +
  geom_col(position = 'dodge') +
  scale_x_continuous(name = 'Total invested in FLW reduction', breaks = c(0, 500, 1000, 2000, 5000)) +
  scale_y_continuous(name = 'Amount invested in each stage', expand = c(0,0), limits = c(0, 1050)) +
  scale_fill_brewer(type='qual', palette='Set2') +
  ggtitle('Optimal allocation of FLW reduction funds to minimize environmental impact', 'for a number of possible total investments') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'bottom')

ggsave(file.path(fpfig, 'sixstage_costcurve_allocations4impacts_barplot.png'), height = 9, width = 9, dpi = 300)

# Compare optimization results to the baseline.
optimal_value_all <- read.csv(file.path(fp_output, 'sixstage_scenario_opt_values.csv'), stringsAsFactors = FALSE)

# Add baseline values.
baseline <- read.csv(file.path(fp_output, 'sixstage_scenario_grid_lcia_results.csv'), stringsAsFactors = FALSE) %>%
  filter(rowSums(.[,1:6]) == 0)

baseline <- baseline %>% 
  filter(grepl('gcc|land|watr|enrg', impact_category)) %>%
  mutate(category = c('energy', 'GHG', 'land', 'water'), total_cost = 0)

optimal_value_all <- optimal_value_all %>%
  rbind(baseline[,c('category','total_cost','value')]) %>%
  group_by(category) %>%
  mutate(value = value/max(value))

ggplot(optimal_value_all, aes(x = total_cost, y = value, color = category)) + 
  geom_point() + geom_line() +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0,500,1000,2000,5000), name = 'Total invested (million $)') +
  scale_y_continuous(labels = scales::percent, name = 'Impact (percent of baseline)') +
  scale_color_brewer(name = 'Impact category', type = 'qual', palette = 'Set2') +
  ggtitle('Impact reduction by total invested', 'optimizing for each impact category separately')

ggsave(file.path(fpfig, 'sixstage_impactreduction_bytotalinvested.png'), height = 6, width = 6, dpi = 300)

# Cost curves based on ReFED data
# ===============================

sectorpars <- read.csv(file.path(fp_output, 'sector_parameters.csv'), stringsAsFactors = FALSE)

w <- function(x, W0, Wu, B, ...) {
  2*(W0-Wu)/(exp(B*x) + 1) + Wu
}

sectorpars_final <- with(sectorpars, data.frame(W0 = W0, Wu = Wu_final, B = B_final))
xmax <- 200

yvals_curves <- pmap(sectorpars, w, x = seq(0, xmax, 1))
yvals_curves_final <- pmap(sectorpars_final, w, x = seq(0, xmax, 1))
dat_for_curves <- imap_dfr(yvals_curves, ~ data.frame(sector = sectorpars$sector_name[.y], stage = sectorpars$stage[.y], x = seq(0, xmax, 1), y = .x))
dat_for_curves_household <- imap_dfr(yvals_curves_final, ~ data.frame(sector = paste(sectorpars$sector_name[.y], 'final'), code = sectorpars$stage_code_final[.y], x = seq(0, xmax, 1), y = .x)) %>%
  filter(code %in% 'L5') %>%
  mutate(stage = 'household') %>%
  select(sector, stage, x, y)
dat_for_curves <- rbind(dat_for_curves, dat_for_curves_household)

ggplot(dat_for_curves, aes(x = x, y = y, color = stage, group = sector)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(type='qual', palette='Set2') +
  scale_x_continuous(name = 'Cost (million $)', expand = c(0,0), limits = c(0, xmax)) +
  scale_y_continuous(name = 'Waste rate', expand = c(0,0), labels = scales::percent, limits = c(0, 0.26)) +
  ggtitle('Cost curves for each FSC sector from ReFED data', 'Each stage has multiple sectors')

# Show averages over all the sectors
dat_for_curves %>%
  group_by(stage, x) %>%
  summarize(y = mean(y)) %>%
ggplot(aes(x = x, y = y, color = stage)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(type='qual', palette='Set2') +
  scale_x_continuous(name = 'Cost (million $)', expand = c(0,0), limits = c(0, 100)) +
  scale_y_continuous(name = 'Waste rate', expand = c(0,0), labels = scales::percent, limits = c(0, 0.15)) +
  ggtitle('Cost curves for each FSC stage from ReFED data', 'Average curve over all sectors in each stage')
ggsave('/nfs/qread-data/figures/sixstage_costcurve_refedcurvesbystage.png', height = 6, width = 6, dpi = 300)
