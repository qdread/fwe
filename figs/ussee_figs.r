# Figures specifically for USSEE powerpoint
# Black or white theme
# QDR / FWE / 28 June 2019


library(tidyverse)
library(gridExtra)
fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')
fp_github <- ifelse(dir.exists('~/Documents/GitHub/'), '~/Documents/GitHub/fwe', '~/fwe')

source(file.path(fp_github, 'figs/theme_black.R'))

stage_full_names <- c('production', 'processing', 'retail', 'consumption: foodservice', 'consumption: institutional', 'consumption: household')

stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfoodservice', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')


# Individual cost curves for each intervention separately.
# ========================================================

sectorpars <- read.csv(file.path(fp_output, 'sector_parameters.csv'), stringsAsFactors = FALSE)

w <- function(x, W0, Wu, B, ...) {
  2*(W0-Wu)/(exp(B*x) + 1) + Wu
}

sectorpars_final <- with(sectorpars, data.frame(W0 = W0, Wu = Wu_final, B = B_final))
xmax <- 10
incr <- 0.01 # Increase to improve resolution

# yvals_curves <- pmap(sectorpars, w, x = seq(0, xmax, incr))
# yvals_curves_final <- pmap(sectorpars_final, w, x = seq(0, xmax, incr))
# dat_for_curves <- imap_dfr(yvals_curves, ~ data.frame(sector = sectorpars$sector_name[.y], stage = sectorpars$stage[.y], x = seq(0, xmax, incr), y = .x))
# dat_for_curves_household <- imap_dfr(yvals_curves_final, ~ data.frame(sector = paste(sectorpars$sector_name[.y], 'final'), code = sectorpars$stage_code_final[.y], x = seq(0, xmax, incr), y = .x)) %>%
#   filter(code %in% 'L5') %>%
#   mutate(stage = 'household') %>%
#   select(sector, stage, x, y)
# dat_for_curves <- rbind(dat_for_curves, dat_for_curves_household)
# 
# dat_for_curves$stage <- factor(dat_for_curves$stage, levels = c('agriculture', 'processing', 'retail', 'foodservice', 'institutional', 'household'), labels = stage_full_names)
# 
# # Show averages over all the sectors
# # Plot each one on a separate file.
# 
# # mean
# plotdat <- dat_for_curves %>%
#   group_by(stage, x) %>%
#   summarize(y = mean(y)) %>%
#   group_by(stage) %>%
#   mutate(xlimmax = 1 + min(x[y - last(y) <= 0.005])) %>%
#   filter(x <= xlimmax)
# 
# # first
# plotdat <- dat_for_curves %>%
#   group_by(stage) %>%
#   filter(sector %in% first(sector)) %>%
#   mutate(xlimmax = 1 + min(x[y - last(y) <= 0.002])) %>%
#   filter(x <= xlimmax)

# the one where W0 and Wu have a median difference
# start with sectorpars again
sectorpars_mediandifference <- sectorpars %>%
  mutate(diff = W0 - Wu) %>%
  group_by(stage) %>%
  filter(abs(diff - median(diff)) == min(abs(diff - median(diff)))) %>%
  filter(sector_name %in% first(sector_name)) %>%
  ungroup
sectorpars_final_mediandifference <- sectorpars %>%
  filter(stage_code_final %in% 'L5') %>%
  mutate(diff = W0 - Wu_final) %>%
  filter(abs(diff - median(diff)) == min(abs(diff - median(diff)))) %>%
  filter(sector_name %in% first(sector_name)) %>%
  mutate(Wu = Wu_final, B = B_final, stage = 'household', stage_code ='L5')
sectorpars_median <- rbind(sectorpars_mediandifference, sectorpars_final_mediandifference)

yvals_curves <- pmap(sectorpars_median, w, x = seq(0, xmax, incr))
dat_for_curves <- imap_dfr(yvals_curves, ~ data.frame(sector = sectorpars_median$sector_name[.y], stage = sectorpars_median$stage[.y], x = seq(0, xmax, incr), y = .x))
dat_for_curves$stage <- factor(dat_for_curves$stage, levels = c('agriculture', 'processing', 'retail', 'foodservice', 'institutional', 'household'), labels = stage_full_names)

plotdat <- dat_for_curves %>%
  group_by(stage) %>%
  mutate(xlimmax = 1 + min(x[y - last(y) <= 0.002])) %>%
  filter(x <= xlimmax)


cols <- RColorBrewer::brewer.pal(6, 'Set2')
plotdat$linecol <- cols[as.numeric(plotdat$stage)]

curveplots <- plotdat %>%
  do(p = 
  ggplot(., aes(x = x, y = y)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_color_brewer(type='qual', palette='Set2') +
  scale_x_continuous(name = 'Cost (million $)', expand = c(0,0)) +
  scale_y_continuous(name = 'Waste rate', expand = c(0.05, 0.01), labels = scales::percent_format(accuracy = 0.1)) )

bwcurveplots <- plotdat %>%
  do(p = 
       ggplot(., aes(x = x, y = y)) +
       geom_line(size = 1, color = .$linecol[1]) +
       scale_color_brewer(type='qual', palette='Set2') +
       scale_x_continuous(name = 'Cost (million $)', expand = c(0,0)) +
       scale_y_continuous(name = 'Waste rate', expand = c(0.05, 0.01), labels = scales::percent_format(accuracy = 0.1)) +
       theme_black())


grid.arrange(grobs = curveplots$p, nrows=2)
grid.arrange(grobs = bwcurveplots$p, nrows=2)

# Save each one as a fig.
prefixes <- c('stage1_production', 'stage2_processing', 'stage3_retail', 'stage4_foodservice', 'stage5_institutional', 'stage6_household')

walk2(bwcurveplots$p, prefixes, ~ ggsave(file.path(fpfig, 'ussee', paste0(.y, 'costcurve.png')), .x, height = 2, width = 2.3, dpi = 300))

# 50% reduction plots with error bars
# ===================================

grid_result <- read.csv(file.path(fp_output, 'sixstage_scenario_grid_lcia_results.csv'), stringsAsFactors = FALSE)
grid_sensitivity_df <- read.csv(file.path(fp_output, 'sensitivity_grid_alldraws.csv'), stringsAsFactors = FALSE)
grid_sensitivity_CIs <- read.csv(file.path(fp_output, 'sensitivity_grid_CIs.csv'), stringsAsFactors = FALSE)


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
reduction_plot_with_errorbars <- function(sequence, yval, yminval, ymaxval) {
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
    geom_line(size = 1, color = 'white') +
    geom_errorbar(width = 0.2, color = 'gray60') +
    geom_point(size = 3, color = 'black', fill = 'white', shape = 21, stroke = 2) +
    geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25, color = 'white') +
    scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=1)) +
    coord_cartesian(ylim = c(0.77, 1.02), expand = TRUE) +
    theme_black() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
}


# Draw the plots for each impact category
co2_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('co2', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975)
land_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('land', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975)
water_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('watr', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975)
energy_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('enrg', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975) 
eutr_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('eutr', impact_category)) %>% 
  reduction_plot_with_errorbars(value, q025, q975)

# Save the plots

ggsave(file.path(fpfig, 'ussee/sixstage_gridwithci_co2by50pct.png'), co2_seqplot_ci, height = 5, width = 5.6, dpi = 300)
ggsave(file.path(fpfig, 'ussee/sixstage_gridwithci_landby50pct.png'), land_seqplot_ci, height = 5, width = 5.6, dpi = 300)
ggsave(file.path(fpfig, 'ussee/sixstage_gridwithci_waterby50pct.png'), water_seqplot_ci, height = 5, width = 5.6, dpi = 300)
ggsave(file.path(fpfig, 'ussee/sixstage_gridwithci_energyby50pct.png'), energy_seqplot_ci, height = 5, width = 5.6, dpi = 300)
ggsave(file.path(fpfig, 'ussee/sixstage_gridwithci_eutrophicationby50pct.png'), eutr_seqplot_ci, height = 5, width = 5.6, dpi = 300)

# Create blank plot so that I can build the slides
blank_seqplot <- ggplot(data = data.frame(nbypct = c(0,6), y = c(0.75,1)), aes(x = nbypct, y = y)) +
  scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=1)) +
  coord_cartesian(ylim = c(0.77, 1.02), expand = TRUE) +
  theme_black() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave(file.path(fpfig, 'ussee/sixstage_blankgrid.png'), blank_seqplot, height = 5, width = 5.6, dpi = 300)



# Save figs with larger legends -------------------------------------------

# Function to create reduction plot with error bars
reduction_plot_with_errorbars_v2 <- function(sequence, yval, yminval, ymaxval) {
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
    geom_line(size = 1, color = 'white') +
    geom_errorbar(width = 0.2, color = 'gray60') +
    geom_point(size = 3, color = 'black', fill = 'white', shape = 21, stroke = 2) +
    geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25, color = 'white', size = 6) +
    scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=1)) +
    coord_cartesian(ylim = c(0.77, 1.02), expand = TRUE) +
    theme_black() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size = 21),
          axis.title.x = element_text(size = 21),
          axis.text.y = element_text(size = 21),
          axis.title.y = element_text(size = 21))
  
}

co2_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('co2', impact_category)) %>% 
  reduction_plot_with_errorbars_v2(value, q025, q975)
land_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('land', impact_category)) %>% 
  reduction_plot_with_errorbars_v2(value, q025, q975)
water_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('watr', impact_category)) %>% 
  reduction_plot_with_errorbars_v2(value, q025, q975)
energy_seqplot_ci <- trueseq_withcis %>% 
  filter(grepl('enrg', impact_category)) %>% 
  reduction_plot_with_errorbars_v2(value, q025, q975) 

blank_seqplot <- ggplot(data = data.frame(nbypct = c(0,6), y = c(0.75,1)), aes(x = nbypct, y = y)) +
  scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=1)) +
  coord_cartesian(ylim = c(0.77, 1.02), expand = TRUE) +
  theme_black() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 21),
        axis.title.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),
        axis.title.y = element_text(size = 21))

ggsave(file.path(fpfig, 'secondyeartalk/sixstage_gridwithci_co2by50pct.png'), co2_seqplot_ci, height = 7.75, width = 5.6*7.75/5, dpi = 300)
ggsave(file.path(fpfig, 'secondyeartalk/sixstage_gridwithci_landby50pct.png'), land_seqplot_ci, height = 7.75, width = 5.6*7.75/5, dpi = 300)
ggsave(file.path(fpfig, 'secondyeartalk/sixstage_gridwithci_waterby50pct.png'), water_seqplot_ci, height = 7.75, width = 5.6*7.75/5, dpi = 300)
ggsave(file.path(fpfig, 'secondyeartalk/sixstage_gridwithci_energyby50pct.png'), energy_seqplot_ci, height = 7.75, width = 5.6*7.75/5, dpi = 300)
ggsave(file.path(fpfig, 'secondyeartalk/sixstage_blankgrid.png'), blank_seqplot, height = 7.75, width = 5.6*7.75/5, dpi = 300)


# Create table of ranks ---------------------------------------------------
tab <- read.csv(file.path(fp_output, 'sensitivity_grid_summarytable.csv'))

tab %>%
  mutate(stage_reduced = factor(stage_reduced, levels = stage_full_names),
         mean_rank = round(mean_rank)) %>%
  select(-proportion_not_swapped) %>%
  spread(impact_category, mean_rank)


