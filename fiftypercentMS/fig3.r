# Figure 3 in MS
# Code taken from sixstage_sens_figs.r
# QDR / FWE / 11 Dec 2019

library(tidyverse)
fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')

stage_full_names <- c('production', 'processing', 'retail', 'consumption: foodservice', 'consumption: institutional', 'consumption: household')

optimal_df_all <- read.csv(file.path(fp_output, 'sixstage_scenario_opt_results.csv'))
grid_result <- read.csv(file.path(fp_output, 'sixstage_scenario_grid_lcia_results.csv'), stringsAsFactors = FALSE)
grid_sensitivity_df <- read.csv(file.path(fp_output, 'sensitivity_grid_alldraws.csv'), stringsAsFactors = FALSE)
grid_sensitivity_CIs <- read.csv(file.path(fp_output, 'sensitivity_grid_CIs.csv'), stringsAsFactors = FALSE)

stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfoodservice', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')

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
    scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=1)) +
    coord_cartesian(ylim = c(0.77, 1.02), expand = TRUE) +
    ggtitle(plot_title, plot_subtitle) +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.subtitle = element_text(size = 10))
  
}

subtitle <- 'done by sequentially reducing waste in the FSC stage \nwith greatest impact by 50%'

# Do this as a facet wrap plot

facetplotdat <- trueseq_withcis %>%
  filter(grepl('co2|eutr|land|watr|enrg', impact_category)) %>%
  group_by(impact_category) %>%
  mutate(value_norm = value/max(value),
         q025_norm = q025/max(value),
         q975_norm = q975/max(value)) %>%
  ungroup %>%
  mutate(stage_reduced = stage_full_names_lookup[stage_reduced],
         impact_category = factor(impact_category, labels = c('eutrophication potential', 'greenhouse warming potential', 'energy use', 'land use', 'water use'))) 

# Reorder the facet label factor.
facetplotdat <- facetplotdat %>%
  mutate(impact_category = factor(impact_category, levels = levels(impact_category)[c(3,1,2,4,5)]))

facetplot <- ggplot(facetplotdat %>% mutate(letter = letters[1:5][impact_category]), aes(x = nbypct, y = value_norm, ymin = q025_norm, ymax = q975_norm)) +
  geom_line(size = 1) +
  geom_errorbar(width = 0.2, color = 'gray60') +
  geom_point(size = 3, color = 'white', fill = 'black', shape = 21, stroke = 2) +
  geom_text(aes(label = stage_reduced), angle = 45, hjust = 1.25, size = 2.25) +
  geom_text(aes(label = letter), x = 0, y = 0.79, size = 10) +
  scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy = 1L)) +
  facet_wrap(~ impact_category, nrow = 3) +
  coord_cartesian(ylim = c(0.77, 1.02), expand = TRUE) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(size = 10),
        strip.background = element_blank())

ggsave(file.path(fpfig, 'stoten_ms/fig3.png'), facetplot, height = 9.5 * 2/3, width = 9 * 2/3, dpi = 300)
ggsave('~/google_drive/SESYNC Food Waste/Model_MS1/stoten_revision/figures_final/fig3.pdf', facetplot, height = 9.5 * 2/3, width = 9 * 2/3)
