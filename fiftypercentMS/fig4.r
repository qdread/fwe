# New figure 4 in MS: multipanel Figure 4.
# Using code taken from pathway_commodity_plots.r
# QDR / FWE / 11 Dec 2019

# Load data ---------------------------------------------------------------

fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
fp_github <- file.path(ifelse(dir.exists('~/fwe'), '~/fwe', '~/Documents/GitHub/fwe'))
fp_fig <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'figures')

library(tidyverse)
library(ggrepel)

results_by_commodity_df <- read.csv(file.path(fp_output, 'bestpathway_bycommodity.csv'), stringsAsFactors = FALSE)

# Create figures ----------------------------------------------------------


# Map stage, category, and food commodity group labels to more descriptive labels
stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'foodservice', L4b = 'institutions', L5 = 'households')
categories <- c("impact potential/gcc/kg co2 eq", "resource use/land/m2*yr", "resource use/watr/m3", "resource use/enrg/mj", "impact potential/eutr/kg n eq")
category_short <- c('GHG', 'land', 'water', 'energy', 'eutrophication')
food_categories <- c("cereals", "roots_tubers_fresh", "roots_tubers_processed", 
                     "oilseeds_pulses", "fruit_veg_fresh", "fruit_veg_processed", 
                     "meat", "fish_fresh", "fish_processed", "milk", "eggs", "sugar", 
                     "beverages")
food_categories_labels <- c('cereals', 'roots and tubers (fresh)', 'roots and tubers (processed)', 'oilseeds and pulses', 'fruits and vegetables (fresh)', 'fruits and vegetables (processed)', 'meat', 'fish (fresh)', 'fish (processed)', 'milk', 'eggs', 'sugar', 'beverages')

results_by_commodity_df <- results_by_commodity_df %>%
  mutate(category = category_short[match(category, categories)],
         stage_reduced = stage_full_names_lookup[stage_reduced],
         commodity = food_categories_labels[match(commodity, food_categories)]) 

# Add baseline case for plotting
baseline_df <- data.frame(category = 'all', commodity = unique(results_by_commodity_df$commodity), n_stages_reduced = 0, stage_reduced = 'none', impact = 1, baseline_impact = 1, impact_norm = 1, stringsAsFactors = FALSE)

results_by_commodity_df <- rbind(results_by_commodity_df, baseline_df)


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

# Figures showing top 5 as impact averted ---------------------------------

results_by_commodity_df <- results_by_commodity_df %>% 
  mutate(impact_averted = baseline_impact - impact)

# Convert axis values to a more legible unit by dividing by the 2012 USA population to get per capita impact
pop2012 <- 314e6
percapita_results <- results_by_commodity_df %>%
  mutate_at(vars(impact, baseline_impact, impact_averted), ~ if_else(category%in% 'all', ., .x/pop2012))

# Get all top 5 names to assign colors to them
alltop5 <- map(category_short, ~ get_top_n(results_by_commodity_df, .x, 5)) %>% reduce(c) %>% unique
alltop5_colormap <- setNames(RColorBrewer::brewer.pal(length(alltop5), 'Dark2'), alltop5)

plot_impactaverted <- function(dat, the_category, unit_name, n = 5) {
  dat <- dat %>% 
    filter(commodity %in% get_top_n(., the_category, n), category %in% c('all', the_category))
  
  ggplot(dat, aes(x = n_stages_reduced, y = impact_averted, group = commodity)) + 
    geom_line(size = 1, alpha = 0.33) +
    geom_point(aes(color = commodity), size = 3) +
    geom_text_repel(data = dat %>% filter(n_stages_reduced > 0), aes(label = stage_reduced), size = 2) + # Label to see which stages are being averted
    scale_x_continuous(name = 'Number of stages where waste is reduced', breaks = 0:6) +
    scale_y_continuous(name = parse(text = paste('Per~capita~reduction', unit_name, sep = '~')), 
                       limits = c(0, max(dat$impact_averted) * 1.05), expand = c(0, 0),
                       sec.axis = sec_axis(trans = ~ ./dat$baseline_impact[1], name = 'relative to baseline', labels = scales::percent)) +
    scale_color_manual(name = 'Food group', values = alltop5_colormap, guide = guide_legend(nrow = 2)) +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'bottom')
}

lsize = 10

p_ghg <- plot_impactaverted(percapita_results, 'GHG', '(kg~CO^2~eq.~y^-1)') + annotate('text',x=-Inf,y=Inf,hjust=-0.2,vjust=1.2,label='c', size = lsize) + ggtitle('greenhouse warming potential')
p_land <- plot_impactaverted(percapita_results, 'land', '(m^2~y^-1)') + annotate('text',x=-Inf,y=Inf,hjust=-0.2,vjust=1.2,label='d', size = lsize) + ggtitle('land use')
p_water <- plot_impactaverted(percapita_results, 'water', '(m^3~y^-1)') + annotate('text',x=-Inf,y=Inf,hjust=-0.2,vjust=1.2,label='e', size = lsize) + ggtitle('water use')
p_energy <- plot_impactaverted(percapita_results, 'energy', '(MJ~y^-1)') + annotate('text',x=-Inf,y=Inf,hjust=-0.2,vjust=1.2,label='a', size = lsize) + ggtitle('energy use')
p_eutr <- plot_impactaverted(percapita_results, 'eutrophication', '(kg~N~eq.~y^-1)') + annotate('text',x=-Inf,y=Inf,hjust=-0.2,vjust=1.2,label='b', size = lsize) + ggtitle('eutrophication potential')

# Make a legend for all the plots.
library(cowplot)

leg_plot <- ggplot(percapita_results %>% filter(commodity %in% alltop5), aes(x=1, y=1, color=commodity)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(name = 'Food group', values = alltop5_colormap, guide = guide_legend(nrow = 4))

leg <- get_legend(leg_plot)

# Lay out the 5 plots plus legend as a 3 x 2 grid.

th_top <- theme(legend.position = 'none',
                axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(), 
                plot.title = element_text(hjust = 0.5))
th_bottom <- theme(legend.position = 'none',
                   plot.title = element_text(hjust = 0.5))

top_row <- plot_grid(p_energy + th_top, p_eutr + th_top, nrow = 1)
middle_row <- plot_grid(p_ghg + th_top, p_land + th_bottom, nrow = 1, align = 'h')
bottom_row <- plot_grid(p_water + th_bottom, leg, nrow = 1)

whole_plot <- plot_grid(top_row, middle_row, bottom_row, nrow = 3)

ggsave(file.path(fp_fig, 'stoten_ms/fig4.png'), whole_plot, height = 9.5 , width = 9 , dpi = 300)
