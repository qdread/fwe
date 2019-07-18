# Plot LCIA contributions


# Load everything ---------------------------------------------------------



source('~/fwe/USEEIO/load_scenario_data.r')
source('~/fwe/figs/categorylabels.r')
library(tidyverse)

fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')

stage_full_names <- c('production', 'processing', 'retail', 'consumption: food service', 'consumption: institutional', 'consumption: household')

stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfood service', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')

lcia_contr <- read.csv(file.path(fp_output, 'lcia_contributions.csv'), stringsAsFactors = FALSE, check.names = FALSE)


# Data manipulation -------------------------------------------------------

# Get totals for each stage.

lcia_contr_long <- lcia_contr %>%
  select(-scenario) %>%
  gather(industry, value, -stage_reduced, -impact_category) %>%
  mutate(BEA_389_code = toupper(substr(industry, 1, 6)))

# Join the codes with the stage they belong to.
lcia_contr_long <- lcia_contr_long %>%
  left_join(naics_foodsystem %>% select(BEA_389_code, stage_code)) %>%
  mutate(stage_code = if_else(is.na(stage_code), 'other', stage_code))

# Sum up
lcia_contr_sums <- lcia_contr_long %>%
  group_by(stage_reduced, impact_category, stage_code) %>%
  summarize(value = sum(value))


# Plots of waste impacts by category and stage ----------------------------



# Make a plot
lcia_contr_sums %>%
  filter(stage_reduced %in% 'baseline', 
         grepl('land|watr|co2|eutr|enrg', impact_category) ) %>%
  ggplot(aes(x = stage_code, y = value)) +
    geom_col() +
    facet_wrap(~ impact_category, scales = 'free_y') +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw()

# Create a difference with the waste impacts.
lcia_contr_diffs <- lcia_contr_sums %>%
  ungroup %>%
  spread(stage_reduced, value) %>%
  select(impact_category, stage_code, baseline, zerowaste)

# Make another plot
lcia_contr_diffs %>%
  filter(grepl('land|watr|co2|eutr|enrg', impact_category) ) %>%
  ggplot(aes(x = stage_code)) +
  geom_col(aes(y = baseline), fill = 'indianred') +
  geom_col(aes(y = zerowaste), fill = 'gray30') +
  facet_wrap(~ impact_category, scales = 'free_y') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels = c('production', 'processing', 'retail', 'food service', 'institutional', 'other (including\nhouseholds)')) +
  theme_bw()

label_data <- tibble(impact_category = unique(lcia_contr_diffs$impact_category),
                     category_label = category_labels,
                     category_label_character = category_labels_character,
                     conversion_factor = conversion_factor)

# Make plots each separately
label_names <- c('production', 'processing', 'retail', 'food service', 'institutional', 'other (including\nhouseholds)')

diff_plots <- lcia_contr_diffs %>%
  left_join(label_data) %>%
  mutate(baseline = baseline * conversion_factor, zerowaste = zerowaste * conversion_factor) %>%
  filter(grepl('land|watr|co2|eutr|enrg', impact_category) ) %>%
  group_by(impact_category) %>%
  do(p = ggplot(., aes(x = stage_code)) +
       geom_col(aes(y = baseline), fill = 'skyblue') +
       geom_col(aes(y = zerowaste), fill = 'blue4') +
       scale_y_continuous(expand = c(0, 0), limits = c(0, max(.$baseline) * 1.05), name = .$category_label[[1]]) +
       scale_x_discrete(labels = label_names, name = 'Stage') +
       theme_bw()) 

# Create a separate plot for energy with a broken axis, since the log scale does not show the difference enough.
# See https://stackoverflow.com/questions/44694496/y-break-with-scale-change-in-r

energy_plotdat <- lcia_contr_diffs %>% 
  filter(grepl('enrg', impact_category)) %>%
  mutate(baseline = baseline/1e9, zerowaste = zerowaste/1e9)
#Function to transform data to y positions
trans <- function(x, break_value, factor) {pmin(x,break_value) + factor*pmax(x-break_value,0)}
yticks <- c(0, 5, 10, 5000, 10000)

#Transform the data onto the display scale
energy_plotdat <- energy_plotdat %>%
  mutate(baseline_t = trans(baseline, 10, 0.0005), zerowaste_t = trans(zerowaste, 10, 0.0005))

energy_plot <- ggplot(energy_plotdat, aes(x = stage_code)) +
  geom_col(aes(y = baseline_t), fill = 'skyblue') +
  geom_col(aes(y = zerowaste_t), fill = 'blue4') +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=11, ymax=12), fill="white", color = 'gray50') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16), name = category_labels[[16]], breaks = trans(yticks, 10, 0.0005), labels = yticks) +
  scale_x_discrete(labels = label_names, name = 'Stage') +
  theme_bw()

diff_plots$p[[3]] <- energy_plot

# Save plots
walk2(diff_plots$p, c('eutrophication', 'ghg', 'energy', 'land', 'water'),
     ~ ggsave(file.path(fpfig, 'contributionplots', paste0('waste_impact_', .y, '.png')), .x, height = 5.5, width = 5.5, dpi = 300))

# Plot dividing waste impacts by stage ------------------------------------



# Only show waste impacts
# Percentage of direct waste impacts that come from each stage
lcia_contr_diffs %>%
  mutate(waste_impact = baseline - zerowaste) %>%
  filter(grepl('land|watr|co2|eutr|enrg', impact_category) ) %>%
  group_by(impact_category) %>%
  mutate(proportion_waste_impact = waste_impact / sum(waste_impact)) %>%
  ggplot(aes(x = impact_category, y = proportion_waste_impact, fill = stage_code)) +
    geom_col(position = 'stack') +
    scale_fill_manual(name = 'Stage', labels = label_names, values = c(RColorBrewer::brewer.pal(5, 'Set2'), 'gray50')) +
    scale_x_discrete(name = 'Impact category', labels = c('eutrophication', 'GHG', 'energy', 'land', 'water')) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1.01), name = 'Proportion of direct waste impacts from each stage') +
    theme_bw() +
    theme(legend.position = 'bottom')

ggsave(file.path(fpfig, 'contributionplots/directwasteimpacts.png'), height = 6, width = 5, dpi = 300)

# Split up impacts by food type -------------------------------------------

# Total contributions of each food type in final demand to impacts

lcia_contr_withprops <- lcia_contr_long %>%
  left_join(naics_foodsystem %>% select(BEA_389_code, cereals:beverages))

lcia_impacts_bycategory <- lcia_contr_withprops %>%
  select(cereals:beverages) %>%
  sweep(1, lcia_contr_withprops$value, `*`)

impacts_byfoodtype <- lcia_contr_withprops %>%
  select(stage_reduced:stage_code) %>%
  cbind(lcia_impacts_bycategory) %>%
  group_by(stage_reduced, impact_category) %>%
  summarize_at(vars(cereals:beverages), sum, na.rm = TRUE)

total_impacts_byfoodtype <- impacts_byfoodtype %>%
  filter(stage_reduced %in% c('baseline', 'zerowaste'),
         grepl('land|watr|co2|eutr|enrg', impact_category)) %>%
  mutate(fruit_veg = fruit_veg_fresh + fruit_veg_processed,
         fish = fish_fresh + fish_processed,
         roots_tubers = roots_tubers_fresh + roots_tubers_processed) %>%
  select(-contains('fresh'), -contains('processed'))

total_impacts_long <- total_impacts_byfoodtype %>%
  ungroup %>%
  gather(food_type, value, -stage_reduced, -impact_category) %>%
  spread(stage_reduced, value) %>%
  mutate(waste_impact = baseline - zerowaste)
  
# Plot  
source('~/fwe/figs/categorylabels.r')
label_data <- tibble(impact_category = unique(lcia_contr_diffs$impact_category),
                     category_label = category_labels,
                     category_label_character = category_labels_character,
                     conversion_factor = conversion_factor)


p_impact <- total_impacts_long %>%
  left_join(label_data) %>%
  select(-baseline) %>%
  mutate(food_type = factor(food_type, levels = c('cereals', 'fruit_veg', 'oilseeds_pulses', 'roots_tubers', 'meat', 'milk', 'fish', 'eggs', 'sugar', 'beverages'), labels = c('cereals', 'fruits and\nvegetables', 'oilseeds\nand pulses', 'roots and\ntubers', 'meat', 'dairy', 'fish', 'eggs', 'sugar', 'beverages'))) %>%
  gather(waste, value, -impact_category, -food_type, -category_label, -category_label_character, -conversion_factor) %>%
  mutate(value = value * conversion_factor) %>%
  ggplot(aes(x = food_type, y = value, fill = waste)) +
    geom_col(position = 'stack') +
    facet_wrap(~ category_label_character, scales = 'free_y', labeller = label_parsed) +
    scale_x_discrete(name = 'Food type') +
    scale_y_continuous(name = 'Environmental impact', expand = c(0,0)) +
    theme_bw() +
    theme(strip.background = element_blank(), legend.position = c(0.8, 0.2)) +
    scale_fill_manual(values = c('skyblue', 'blue4'), name = 'Destination', labels = c('wasted', 'not wasted'))

# Add emojis to this plot
# bread, potato, French fries, seedling, peach, canned food, meat on bone, fish, sushi, cheese, egg
emoji_mapping <- data.frame(food_type = c('cereals', 'fruit_veg', 'oilseeds_pulses', 'roots_tubers', 'meat', 'milk', 'fish', 'eggs', 'sugar', 'beverages'),
                            emoji_code = c('1f35e', '1f351', '1f331', '1f954', '1f357', '1f9c0', '1f41f', '1f95a', '1f36d', '1f37a'),
                            stringsAsFactors = FALSE)

library(emoGG)
p_impact + 
  geom_emoji(x = 1, y = -Inf, emoji = '1f35e') +
  geom_emoji(x = 2, y = -Inf, emoji = '1f351') +
  geom_emoji(x = 3, y = -Inf, emoji = '1f331') +
  geom_emoji(x = 4, y = -Inf, emoji = '1f954') +
  geom_emoji(x = 5, y = -Inf, emoji = '1f357') +
  geom_emoji(x = 6, y = -Inf, emoji = '1f9c0') +
  geom_emoji(x = 7, y = -Inf, emoji = '1f41f') +
  geom_emoji(x = 8, y = -Inf, emoji = '1f95a') +
  geom_emoji(x = 9, y = -Inf, emoji = '1f36d') +
  geom_emoji(x = 10, y = -Inf, emoji = '1f37a') 
  
# Split up each plot to a different panel.
impact_plots <- lcia_contr_diffs %>%
  left_join(label_data) %>%
  mutate(baseline = baseline * conversion_factor, zerowaste = zerowaste * conversion_factor) %>%
  filter(grepl('land|watr|co2|eutr|enrg', impact_category) ) %>%
  group_by(impact_category) %>%
  do(p = ggplot(., aes(x = stage_code)) +
       geom_col(aes(y = baseline), fill = 'indianred') +
       geom_col(aes(y = zerowaste), fill = 'gray30') +
       scale_y_continuous(expand = c(0, 0), limits = c(0, max(.$baseline) * 1.05), name = .$category_label[[1]]) +
       scale_x_discrete(labels = label_names, name = 'Stage') +
       theme_bw()) 

# Stupid function to add emojis to the plot
add_emojis <- function(p, ypos) {
  p + 
    geom_emoji(x = 1, y = ypos, emoji = '1f35e') +
    geom_emoji(x = 2, y = ypos, emoji = '1f351') +
    geom_emoji(x = 3, y = ypos, emoji = '1f331') +
    geom_emoji(x = 4, y = ypos, emoji = '1f954') +
    geom_emoji(x = 5, y = ypos, emoji = '1f357') +
    geom_emoji(x = 6, y = ypos, emoji = '1f9c0') +
    geom_emoji(x = 7, y = ypos, emoji = '1f41f') +
    geom_emoji(x = 8, y = ypos, emoji = '1f95a') +
    geom_emoji(x = 9, y = ypos, emoji = '1f36d') +
    geom_emoji(x = 10, y = ypos, emoji = '1f37a') 
}

impact_plots <- total_impacts_long %>%
  left_join(label_data) %>%
  select(-waste_impact) %>%
  mutate(food_type = factor(food_type, levels = c('cereals', 'fruit_veg', 'oilseeds_pulses', 'roots_tubers', 'meat', 'milk', 'fish', 'eggs', 'sugar', 'beverages'), labels = c('cereals', 'fruits and\nvegetables', 'oilseeds\nand pulses', 'roots and\ntubers', 'meat', 'dairy', 'fish', 'eggs', 'sugar', 'beverages'))) %>%
  gather(waste, value, -impact_category, -food_type, -category_label, -category_label_character, -conversion_factor) %>%
  mutate(value = value * conversion_factor) %>%
  group_by(impact_category) %>%
  do(p = add_emojis(ggplot(., aes(x = food_type, y = value, fill = waste)) +
    geom_col(position = 'identity') +
    scale_x_discrete(name = 'Food type') +
    scale_y_continuous(name = .$category_label[[1]], expand = c(0,0), limits = c(0, max(.$value) * 1.1)) +
    scale_fill_manual(values = c('skyblue', 'blue4'), name = 'Destination', labels = c('wasted', 'not wasted')) +
    theme_bw() +
    theme(legend.position = 'none'),
      ypos = max(.$value * 1.05)))

walk2(impact_plots$p, c('eutrophication', 'ghg', 'energy', 'land', 'water'),
      ~ ggsave(file.path(fpfig, 'contributionplots', paste0('food_types_impact_', .y, '.png')), .x, height = 5.5, width = 6.5, dpi = 300))


# Plot impacts per dollar of final demand ---------------------------------

# Probably don't use these plots.

# Load final demand
eeio_demand <- read.csv(file.path(fp_output, 'lcia_contributions_finaldemand.csv'), stringsAsFactors = FALSE)

# Get totals of final demand for each type of food
eeio_demand$BEA_389_code <- toupper(substr(eeio_demand$BEA_389_code,1,6))

get_demand_product <- function(dat) {
  props <- dat %>% 
    select(cereals:beverages) %>%
    as.matrix %>%
    t
  res <- props %*% dat$final_demand
  return(data.frame(food_type = dimnames(res)[[1]], final_demand = as.numeric(res)))
}

eeio_demand_gp <- eeio_demand %>%
  left_join(naics_foodsystem) %>%
  group_by(stage_reduced) %>%
  do(get_demand_product(.))

# Sum up the fresh and processed ones.
eeio_demand_gp <- eeio_demand_gp %>%
  spread(food_type, final_demand) %>%
  mutate(fruit_veg = fruit_veg_fresh + fruit_veg_processed,
         fish = fish_fresh + fish_processed,
         roots_tubers = roots_tubers_fresh + roots_tubers_processed) %>%
  select(-contains('fresh'), -contains('processed')) %>%
  ungroup %>%
  gather(food_type, final_demand, -stage_reduced)

baseline_demand <- eeio_demand_gp %>%
  filter(stage_reduced %in% 'baseline') %>%
  select(-stage_reduced)

# Divide everything by baseline demand

impact_plots_byoutput <- total_impacts_byfoodtype %>%
  ungroup %>%
  gather(food_type, value, -stage_reduced, -impact_category) %>%
  left_join(baseline_demand) %>%
  left_join(label_data) %>%
  mutate(impact_per_output = value / final_demand) %>%
  mutate(food_type = factor(food_type, levels = c('cereals', 'fruit_veg', 'oilseeds_pulses', 'roots_tubers', 'meat', 'milk', 'fish', 'eggs', 'sugar', 'beverages'), labels = c('cereals', 'fruits and\nvegetables', 'oilseeds\nand pulses', 'roots and\ntubers', 'meat', 'dairy', 'fish', 'eggs', 'sugar', 'beverages'))) %>%
  group_by(impact_category) %>%
  do(p = add_emojis(ggplot(., aes(x = food_type, y = impact_per_output, fill = stage_reduced)) +
                      geom_col(position = 'identity') +
                      scale_x_discrete(name = 'Food type') +
                      scale_y_continuous(name = .$category_label[[1]], expand = c(0,0), limits = c(0, max(.$impact_per_output) * 1.1)) +
                      scale_fill_manual(values = c('skyblue', 'blue4'), name = 'Destination', labels = c('wasted', 'not wasted')) +
                      theme_bw() +
                      theme(legend.position = 'none'),
                    ypos = max(.$impact_per_output * 1.05)))

# This may not be the best. Let's instead look at the total GROSS output of all the sectors.
# Also multiply this by proportion food.
M <- read.csv(file.path(fp_bea, 'make2012.csv'), row.names = 1, check.names = FALSE)
gross_outputs <- M[sector_short_names, 'T008']
gross_outputs_adj <- gross_outputs * naics_foodsystem$proportion_food # in millions of dollars.

# Multiply gross outputs by food types.
props <- naics_foodsystem %>% 
  select(cereals:beverages) %>%
  as.matrix %>%
  t
gross_output_byfoodtype <- props %*% gross_outputs_adj
baseline_grossoutput <- data.frame(food_type = dimnames(gross_output_byfoodtype)[[1]],
                                   gross_output_adj = as.numeric(gross_output_byfoodtype))

impact_plots_bygrossoutput <- total_impacts_byfoodtype %>%
  ungroup %>%
  gather(food_type, value, -stage_reduced, -impact_category) %>%
  left_join(baseline_grossoutput) %>%
  left_join(label_data) %>%
  mutate(impact_per_output = value / gross_output_adj) %>%
  mutate(food_type = factor(food_type, levels = c('cereals', 'fruit_veg', 'oilseeds_pulses', 'roots_tubers', 'meat', 'milk', 'fish', 'eggs', 'sugar', 'beverages'), labels = c('cereals', 'fruits and\nvegetables', 'oilseeds\nand pulses', 'roots and\ntubers', 'meat', 'dairy', 'fish', 'eggs', 'sugar', 'beverages'))) %>%
  group_by(impact_category) %>%
  do(p = add_emojis(ggplot(., aes(x = food_type, y = impact_per_output, fill = stage_reduced)) +
                      geom_col(position = 'identity') +
                      scale_x_discrete(name = 'Food type') +
                      scale_y_continuous(name = .$category_label[[1]], expand = c(0,0), limits = c(0, max(.$impact_per_output) * 1.1)) +
                      scale_fill_manual(values = c('skyblue', 'blue4'), name = 'Destination', labels = c('wasted', 'not wasted')) +
                      theme_bw() +
                      theme(legend.position = 'none'),
                    ypos = max(.$impact_per_output * 1.05)))
