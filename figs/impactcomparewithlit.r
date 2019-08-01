# Figure comparing our estimates for total baseline impact due to FLW with the literature
# Differences can result from either a difference in impact values or food loss and waste rates.

library(tidyverse)
library(units)
fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')

waste_impacts <- read.csv(file.path(fp_output, 'baseline_impacts.csv'), stringsAsFactors = FALSE)

energy_litvalues <- c(1740, 2030, 2559)
energy_mj <- set_units(set_units(energy_litvalues*1e12, 'BTU'), 'MJ') / 314e6
          
values <- c(405,498,1117,2.7,42,53,54,352,energy_mj,329)

lit_df <- data.frame(impact_category = c('land','land','land','eutrophication','water','water','water','water','energy','energy','energy','GHG'),
                     source = 'literature',
                     percapita_waste_impact = values)

waste_impacts <- waste_impacts %>% 
  filter(grepl('land|watr|enrg|eutr|co2', impact_category)) %>%
  mutate(impact_category = c('eutrophication','GHG','energy','land','water'),
         source = 'this study') %>%
  select(impact_category, source, percapita_waste_impact) %>%
  rbind(lit_df) %>%
  mutate(percapita_waste_impact = if_else(impact_category=='energy',percapita_waste_impact/1000, percapita_waste_impact))

ggplot(waste_impacts, aes(x = 1, y = percapita_waste_impact, fill = source, alpha = source)) +
  geom_point(pch = 21, size = 4) +
  geom_hline(yintercept = 0, color = 'white') +
  facet_wrap(~ impact_category, scales = 'free_y', nrow = 1) +
  scale_x_continuous(limits=c(0.95,1.05), expand=c(0,0)) +
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_fill_manual(values = c('black', 'red')) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.line.x = element_blank(),
        strip.background = element_blank())

# do it as a separate plot
category_labels_character <- c('paste("Eutrophication potential (kg N eq.)")',
                               'paste("GHG emissions (kg ", CO[2], " eq.)")',
                               'paste("Energy use (GJ)")',
                               'paste("Land used (", m^2, ")")',
                               'paste("Water used ", (m^3))')

category_labels_expression <- list("Eutrophication potential (kg N eq.)",
                                   expression(paste("GHG emissions (kg ", CO[2], " eq.)")),
                                   "Energy use (GJ)",
                                   expression(paste("Land used (", m^2, ")")),
                                   expression(paste("Water used ", (m^3))))

plotargs <- tibble(impact_category = unique(waste_impacts$impact_category),
                       maxy = c(3, 600, 10, 2000, 400),
                       labelexpr = category_labels_expression)

waste_plots <- waste_impacts %>%
  left_join(plotargs) %>%
  group_by(impact_category) %>%
  do(p = ggplot(., aes(x = 1, y = percapita_waste_impact, fill = source, alpha = source, size = source)) +
       geom_point(pch = 21) +
       geom_hline(yintercept = 0, color = 'white') +
       scale_x_continuous(limits=c(0.95,1.05), expand=c(0,0)) +
       scale_y_continuous(limits=c(0,.$maxy[1]), expand=c(0,0), name = .$labelexpr[[1]]) +
       scale_alpha_manual(values = c(0.3, 1)) +
       scale_size_manual(values = c(3, 5)) +
       scale_fill_manual(values = c('black', 'red')) +
       theme_classic() +
       theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.line.x = element_blank(), legend.position = 'none'))

library(gridExtra)

png(file.path(fpfig, 'impact_comparison_withlit.png'), height = 4, width = 8, res = 300, units = 'in')
grid.arrange(grobs = waste_plots$p, nrow = 1)
dev.off()
