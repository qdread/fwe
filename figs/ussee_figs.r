# Figures specifically for USSEE powerpoint
# Black or white theme
# QDR / FWE / 28 June 2019


library(tidyverse)
fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')
fpfig <- file.path(fp, 'figures') 
fp_output <- file.path(fp, 'scenario_results')

stage_full_names <- c('production', 'processing', 'retail', 'consumption: food service', 'consumption: institutional', 'consumption: household')

stage_full_names_lookup <- c(none = '', L1 = 'production', L2 = 'processing', L3 = 'retail', L4a = 'consumption:\nfood service', L4b = 'consumption:\ninstitutional', L5 = 'consumption:\nhousehold')


# Individual cost curves for each intervention separately.
# ========================================================

sectorpars <- read.csv(file.path(fp_output, 'sector_parameters.csv'), stringsAsFactors = FALSE)

w <- function(x, W0, Wu, B, ...) {
  2*(W0-Wu)/(exp(B*x) + 1) + Wu
}

sectorpars_final <- with(sectorpars, data.frame(W0 = W0, Wu = Wu_final, B = B_final))
xmax <- 10
incr <- 0.1

yvals_curves <- pmap(sectorpars, w, x = seq(0, xmax, incr))
yvals_curves_final <- pmap(sectorpars_final, w, x = seq(0, xmax, incr))
dat_for_curves <- imap_dfr(yvals_curves, ~ data.frame(sector = sectorpars$sector_name[.y], stage = sectorpars$stage[.y], x = seq(0, xmax, incr), y = .x))
dat_for_curves_household <- imap_dfr(yvals_curves_final, ~ data.frame(sector = paste(sectorpars$sector_name[.y], 'final'), code = sectorpars$stage_code_final[.y], x = seq(0, xmax, incr), y = .x)) %>%
  filter(code %in% 'L5') %>%
  mutate(stage = 'household') %>%
  select(sector, stage, x, y)
dat_for_curves <- rbind(dat_for_curves, dat_for_curves_household)

dat_for_curves$stage <- factor(dat_for_curves$stage, labels = stage_full_names)

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
  geom_line(size = 1) +
  theme_bw() +
  scale_color_brewer(type='qual', palette='Set2') +
  scale_x_continuous(name = 'Cost (million $)', expand = c(0,0), limits = c(0, 5)) +
  scale_y_continuous(name = 'Waste rate', expand = c(0,0), labels = scales::percent, limits = c(0, 0.15)) +
  ggtitle('Cost curves for each FSC stage from ReFED data', 'Average curve over all sectors in each stage') +
  theme(legend.position = 'bottom')

ggsave(file.path(fp, 'figures/sixstage_costcurve_refedcurvesbystage.png'), height = 7, width = 6, dpi = 300)

# Because of the odd look of the figure above, try to just plot a typical or representative sector from each stage.
# The one with the median initial waste value

dat_for_curves %>%
  group_by(stage) %>%
  filter(sector %in% sector[which.min(abs(y - median(y[x==0])))]) %>%
  ggplot(aes(x = x, y = y, color = stage)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_color_brewer(type='qual', palette='Set2') +
  scale_x_continuous(name = 'Cost (million $)', expand = c(0,0), limits = c(0, 5)) +
  scale_y_continuous(name = 'Waste rate', expand = c(0,0), labels = scales::percent, limits = c(0, 0.15)) +
  ggtitle('Cost curves for each FSC stage from ReFED data', 'One representative sector shown for each stage') +
  theme(legend.position = 'bottom')

dat_for_curves %>%
  group_by(stage) %>%
  filter(sector %in% first(sector)) %>%
  ggplot(aes(x = x, y = y, color = stage)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_color_brewer(type='qual', palette='Set2') +
  scale_x_continuous(name = 'Cost (million $)', expand = c(0,0), limits = c(0, 5)) +
  scale_y_continuous(name = 'Waste rate', expand = c(0,0), labels = scales::percent, limits = c(0, 0.15)) +
  ggtitle('Cost curves for each FSC stage from ReFED data', 'One representative sector shown for each stage') +
  theme(legend.position = 'bottom')
