# Figures for "best" reduction pathway by commodity
# QDR / FWE / 23 Sep 2019


# Load data ---------------------------------------------------------------

fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
fp_github <- file.path(ifelse(dir.exists('~/fwe'), '~/fwe', '~/Documents/GitHub/fwe'))
fp_fig <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'figures')

library(tidyverse)

# Load results by commodity
load(file.path(fp_output, 'pathways_by_commodity_output.RData'))

# Load baseline results to normalize all results
baseline_result <- read.csv(file.path(fp_output, 'sixstage_scenario_grid_lcia_results.csv'), stringsAsFactors = FALSE) %>% 
  filter(scenario == 1)


# Process output of pathway script ----------------------------------------

# Convert list columns of tibble into regular columns
# Keep only the environmental good being minimized
results_by_commodity_df <- results_by_commodity %>% 
  pmap_dfr(function(category, commodity, best_pathway, impacts) data.frame(category = category, 
                                                                           commodity = commodity,
                                                                           n_stages_reduced = 1:length(best_pathway),
                                                                           stage_reduced = best_pathway,
                                                                           impact = map_dbl(impacts, ~ .x$value[.x$impact_category == category]))) %>%
  left_join(baseline_result %>% select(impact_category, value), by = c('category'='impact_category')) %>% 
  rename(baseline_impact = value) %>%
  mutate(impact_norm = impact / baseline_impact)

# Save this output to be loaded later
write.csv(results_by_commodity_df, file.path(fp_output, 'bestpathway_bycommodity.csv'), row.names = FALSE)


# Create figures ----------------------------------------------------------

results_by_commodity_df <- read.csv(file.path(fp_output, 'bestpathway_bycommodity.csv'), stringsAsFactors = FALSE)

# Map stage and category labels to more descriptive labels
stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfood service', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')
categories <- c("impact potential/gcc/kg co2 eq", "resource use/land/m2*yr", "resource use/watr/m3", "resource use/enrg/mj", "impact potential/eutr/kg n eq")
category_short <- c('GHG', 'land', 'water', 'energy', 'eutrophication')

results_by_commodity_df <- results_by_commodity_df %>%
  mutate(category = category_short[match(category, categories)],
         stage_reduced = stage_full_names_lookup[stage_reduced]) 

# Add baseline case for plotting
baseline_df <- data.frame(category = 'all', commodity = unique(results_by_commodity_df$commodity), n_stages_reduced = 0, stage_reduced = 'none', impact = 1, baseline_impact = 1, impact_norm = 1, stringsAsFactors = FALSE)

results_by_commodity_df <- rbind(results_by_commodity_df, baseline_df)


# Make a figure for GHG
ggplot(results_by_commodity_df %>% filter(category %in% c('all','GHG')), aes(x = n_stages_reduced, y = impact_norm, group = commodity, fill = commodity)) + 
  geom_line(size = 1) +
  geom_point(size = 3, color = 'white', shape = 21, stroke = 2) +
  scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=0.1)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(size = 10))

ggplot(results_by_commodity_df %>% filter(category %in% c('all','land')), aes(x = n_stages_reduced, y = impact_norm, group = commodity, fill = commodity)) + 
  geom_line(size = 1) +
  geom_point(size = 3, color = 'white', shape = 21, stroke = 2) +
  scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=0.1)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(size = 10))

ggplot(results_by_commodity_df %>% filter(category %in% c('all','water')), aes(x = n_stages_reduced, y = impact_norm, group = commodity, fill = commodity)) + 
  geom_line(size = 1) +
  geom_point(size = 3, color = 'white', shape = 21, stroke = 2) +
  scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=0.1)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(size = 10))

ggplot(results_by_commodity_df %>% filter(category %in% c('all','energy')), aes(x = n_stages_reduced, y = impact_norm, group = commodity, fill = commodity)) + 
  geom_line(size = 1) +
  geom_point(size = 3, color = 'white', shape = 21, stroke = 2) +
  scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
  scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=0.1)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(size = 10))



# Figures showing only the top 5 commodities ------------------------------

# Modify so we can show other numbers besides 5
get_top_n <- function(data, the_category, n_comm = 5) {
  data %>% 
    filter(category %in% the_category) %>%
    group_by(commodity) %>%
    filter(n_stages_reduced == max(n_stages_reduced)) %>%
    ungroup %>%
    arrange(impact_norm) %>%
    slice(1:n_comm) %>%
    pull(commodity)
}

plot_top_n <- function(data, the_category, n = 5) {
  data %>% 
    filter(commodity %in% get_top_n(., the_category, n), category %in% c('all', the_category)) %>%
    ggplot(aes(x = n_stages_reduced, y = impact_norm, group = commodity, fill = commodity)) + 
    geom_line(size = 1) +
    geom_point(size = 3, color = 'white', shape = 21, stroke = 2) +
    scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = 'Impact relative to baseline', labels = scales::percent_format(accuracy=0.1)) +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.subtitle = element_text(size = 10))
}

plot_top_n(results_by_commodity_df, 'GHG')
plot_top_n(results_by_commodity_df, 'land')
plot_top_n(results_by_commodity_df, 'water')
plot_top_n(results_by_commodity_df, 'energy')
plot_top_n(results_by_commodity_df, 'eutrophication')


# Figures showing top 5 as impact averted ---------------------------------

results_by_commodity_df <- results_by_commodity_df %>% 
  mutate(impact_averted = baseline_impact - impact)

# Convert axis values to a more legible unit by dividing by the 2012 USA population to get per capita impact
pop2012 <- 314e6
percapita_results <- results_by_commodity_df %>%
  mutate_at(vars(impact, baseline_impact, impact_averted), ~ if_else(category%in% 'all', ., .x/pop2012))

# This is a direct unit conversion, probably not needed to do.
# library(units)
# res_wide <- results_by_commodity_df %>%
#   
#   pivot_wider(id_cols = c(commodity, n_stages_reduced, stage_reduced),
#               names_from = category,
#               values_from = c(impact:impact_averted)) %>%
#   # Set initial units
#   mutate_at(vars(contains('GHG')), ~ set_units(.x, 'kg')) %>%
#   mutate_at(vars(contains('land')), ~ set_units(.x, 'm^2')) %>%
#   mutate_at(vars(contains('water')), ~ set_units(.x, 'm^3')) %>%
#   mutate_at(vars(contains('energy')), ~ set_units(.x, 'MJ')) %>%
#   mutate_at(vars(contains('eutrophication')), ~ set_units(.x, 'kg')) %>%
#   # Convert the units
#   mutate_at(vars(contains('GHG')), ~ set_units(.x, 'Mt')) %>%
#   mutate_at(vars(contains('land')), ~ set_units(.x, 'ha')) %>%
#   mutate_at(vars(contains('water')), ~ set_units(.x, 'm^3')) %>%
#   mutate_at(vars(contains('energy')), ~ set_units(.x, 'PJ')) %>%
#   mutate_at(vars(contains('eutrophication')), ~ set_units(.x, 'Mt'))
# source(file.path(fp_github, 'figs/categorylabels.r'))

# Get all top 5 names to assign colors to them
alltop5 <- reduce(map(category_short, ~ get_top_n(results_by_commodity_df, .x, 5)), c) %>% unique
alltop5_colormap <- setNames(RColorBrewer::brewer.pal(length(alltop5), 'Dark2'), alltop5)

plot_impactaverted <- function(dat, the_category, unit_name, n = 5) {
  dat <- dat %>% 
    filter(commodity %in% get_top_n(., the_category, n), category %in% c('all', the_category))

  ggplot(dat, aes(x = n_stages_reduced, y = impact_averted, group = commodity)) + 
    geom_line(size = 1, alpha = 0.33) +
    geom_point(aes(color = commodity), size = 3) +
    geom_text_repel(data = dat %>% filter(n_stages_reduced > 0), aes(label = stage_reduced), size = 2) + # Label to see which stages are being averted
    scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = parse(text = paste('Per~capita~impact~averted:', unit_name, sep = '~')), 
                       limits = c(0, max(dat$impact_averted) * 1.05), expand = c(0, 0),
                       sec.axis = sec_axis(trans = ~ ./dat$baseline_impact[1], name = 'relative to baseline', labels = scales::percent)) +
    scale_color_manual(values = alltop5_colormap) +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.subtitle = element_text(size = 10))
}

p_ghg <- plot_impactaverted(percapita_results, 'GHG', 'GHG~(kg~CO^2~eq.~y^-1)')
p_land <- plot_impactaverted(percapita_results, 'land', 'land~(m^2~y^-1)')
p_water <- plot_impactaverted(percapita_results, 'water', 'water~(m^3~y^-1)')
p_energy <- plot_impactaverted(percapita_results, 'energy', 'energy~(MJ~y^-1)')
p_eutr <- plot_impactaverted(percapita_results, 'eutrophication', 'eutrophication~potential~(kg~N~eq.~y^-1)')

# Draw plots to pngs
ggsave(file.path(fp_fig, 'impact_by_commodity/top5commodities_ghg.png'), p_ghg, height = 5, width = 7, dpi = 300)  
ggsave(file.path(fp_fig, 'impact_by_commodity/top5commodities_land.png'), p_land, height = 5, width = 7, dpi = 300)  
ggsave(file.path(fp_fig, 'impact_by_commodity/top5commodities_water.png'), p_water, height = 5, width = 7, dpi = 300)  
ggsave(file.path(fp_fig, 'impact_by_commodity/top5commodities_energy.png'), p_energy, height = 5, width = 7, dpi = 300)  
ggsave(file.path(fp_fig, 'impact_by_commodity/top5commodities_eutrophication.png'), p_eutr, height = 5, width = 7, dpi = 300)  
