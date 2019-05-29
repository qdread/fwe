# Plot equations to show reduction benefits

library(tidyverse)

# Impact plot varying intensity of stage 1 --------------------------------


final = 1
loss = c(0.5, 0.5, 0.5)
intensity = c(1, 1, 1)

calc_impacts <- function(final, loss, intensity) {
  weights = final * c(1/prod(1-loss[1:3]), 1/prod(1-loss[2:3]), 1/prod(1-loss[3]))
  impacts = intensity * weights
  sum(impacts)
}

calc_impacts(final,loss,intensity)
calc_impacts(final,c(0.25,0.5,0.5), intensity)
calc_impacts(final,c(0.5,0.25,0.5), intensity)
calc_impacts(final,c(0.5,0.5,0.25), intensity)

all_inputs <- expand.grid(final = 1, stage_to_reduce = 1:3, intensity_1 = 1:3, intensity_2 = 1:3, intensity_3 = 1:3)

all_inputs$total_impact <- apply(all_inputs, 1, function(x) {
  loss_x <- c(0.5, 0.5, 0.5)
  loss_x[x['stage_to_reduce']] <- 0.25
  calc_impacts(x['final'],  loss_x , x[c('intensity_1', 'intensity_2', 'intensity_3')])
  })

ggplot(all_inputs %>% filter(intensity_2 == 1, intensity_3 == 1), aes(x = factor(stage_to_reduce), y = total_impact, group = intensity_1, color = factor(intensity_1))) +
  geom_line(size = 1) +
  geom_text(data = data.frame(stage_to_reduce = c(2,2,2), total_impact = c(9, 14, 19), intensity_1 = c(1,2,3), lab = c('I[P] == 1', 'I[P] == 2', 'I[P] == 3')), aes(label = lab), parse = TRUE, size = 9) +
  theme_bw() +
  scale_x_discrete(name = 'Stage where FLW is reduced', labels = c('Producer', 'Retail', 'Consumer'), expand = c(0.05,0.05)) +
  scale_y_continuous(name = 'Total impact', limits = c(8, 23), expand = c(0,0)) +
  theme(legend.position = 'none')

ggsave('~/Dropbox/sesync/firstyear_talk/impact_plot_ip1.png', height = 4, width = 4, dpi = 300)


# Make impact plot black and white and build up ---------------------------

source('~/Documents/R/theme_black.R')

scb <- scale_color_brewer(type = 'qual', palette = 'Set2')

p1 <- ggplot(all_inputs %>% filter(intensity_1 == 1, intensity_2 == 1, intensity_3 == 1), aes(x = factor(stage_to_reduce), y = total_impact, group = intensity_1, color = factor(intensity_1))) +
  geom_line(size = 1) +
  geom_text(data = data.frame(stage_to_reduce = c(2), total_impact = c(9), intensity_1 = c(1), lab = c('I[P] == 1')), aes(label = lab), parse = TRUE, size = 9) +
  scale_x_discrete(name = 'Stage where FLW is reduced', labels = c('Producer', 'Retail', 'Consumer'), expand = c(0.05,0.05)) +
  scale_y_continuous(name = 'Total impact', limits = c(8, 23), expand = c(0,0)) +
  theme_black() +
  theme(legend.position = 'none')

p2 <- ggplot(all_inputs %>% filter(intensity_1 < 3, intensity_2 == 1, intensity_3 == 1), aes(x = factor(stage_to_reduce), y = total_impact, group = intensity_1, color = factor(intensity_1))) +
  geom_line(size = 1) +
  geom_text(data = data.frame(stage_to_reduce = c(2,2), total_impact = c(9, 14), intensity_1 = c(1,2), lab = c('I[P] == 1', 'I[P] == 2')), aes(label = lab), parse = TRUE, size = 9) +
  scale_x_discrete(name = 'Stage where FLW is reduced', labels = c('Producer', 'Retail', 'Consumer'), expand = c(0.05,0.05)) +
  scale_y_continuous(name = 'Total impact', limits = c(8, 23), expand = c(0,0)) +
  theme_black() +
  theme(legend.position = 'none')

p3 <- ggplot(all_inputs %>% filter(intensity_2 == 1, intensity_3 == 1), aes(x = factor(stage_to_reduce), y = total_impact, group = intensity_1, color = factor(intensity_1))) +
  geom_line(size = 1) +
  geom_text(data = data.frame(stage_to_reduce = c(2,2,2), total_impact = c(9, 14, 19), intensity_1 = c(1,2,3), lab = c('I[P] == 1', 'I[P] == 2', 'I[P] == 3')), aes(label = lab), parse = TRUE, size = 9) +
  scale_x_discrete(name = 'Stage where FLW is reduced', labels = c('Producer', 'Retail', 'Consumer'), expand = c(0.05,0.05)) +
  scale_y_continuous(name = 'Total impact', limits = c(8, 23), expand = c(0,0)) +
  theme_black() +
  theme(legend.position = 'none')

ggsave('~/Dropbox/sesync/firstyear_talk/impact_plot_build1.png', p1 + scb, height = 4, width = 4, dpi = 300)
ggsave('~/Dropbox/sesync/firstyear_talk/impact_plot_build2.png', p2 + scb, height = 4, width = 4, dpi = 300)
ggsave('~/Dropbox/sesync/firstyear_talk/impact_plot_build3.png', p3 + scb, height = 4, width = 4, dpi = 300)


# Impact plot varying baseline 1 ------------------------------------------


# Or what if the baseline losses are different between the sectors?
# Baseline loss rates of 0.25 for sectors 2 and 3 in all cases
# Baseline loss rate of 0.25, 0.5, and 0.75 for stage 1 x Reduction rate of 0.25 at each stage
baselines <- list(c(0.25, 0.25, 0.25), c(0.5, 0.25, 0.25), c(0.75, 0.25, 0.25))
calc_impacts(final,baselines[,1],intensity)
calc_impacts(final,baselines[,2],intensity)
calc_impacts(final,baselines[,3],intensity)

inputs2 <- expand.grid(s = 1:3, b = baselines)

inputs2$impact <- apply(inputs2, 1, function(x) {
  x$b[x$s] <- x$b[x$s] * 0.25
  calc_impacts(final, x$b, intensity)
})

inputs2$b1 <- rep(c(0.25, 0.5, 0.75), each = 3)

ggplot(inputs2, aes(x = factor(s), y = impact, group = factor(b1), color = factor(b1))) +
  geom_line(size = 1) +
  geom_text(data = data.frame(s = c(2,2,2), impact = c(5, 5.8, 8.65), b1 = factor(c(0.25, 0.5, 0.75)), lab = c('Baseline[P] == 0.25', 'Baseline[P] == 0.5', 'Baseline[P] == 0.75')), aes(label = lab), parse = TRUE, size = 5.5) +
  scale_x_discrete(name = 'Stage where FLW is reduced', labels = c('Producer', 'Retail', 'Consumer'), expand = c(0.05,0.05)) +
  scale_y_continuous(name = 'Total impact', limits = c(4, 9), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = 'none')
ggsave('~/Dropbox/sesync/firstyear_talk/impact_plot_wp1.png', height = 4, width = 4, dpi = 300)


# Make varying baseline plot black and white and build up -----------------

p1 <- ggplot(inputs2 %>% filter(b1 == 0.25), aes(x = factor(s), y = impact, group = factor(b1), color = factor(b1))) +
  geom_line(size = 1) +
  geom_text(data = data.frame(s = c(2), impact = c(5), b1 = factor(c(0.25)), lab = c('Baseline~L[P] == 0.25')), aes(label = lab), parse = TRUE, size = 5.5) +
  scale_x_discrete(name = 'Stage where FLW is reduced', labels = c('Producer', 'Retail', 'Consumer'), expand = c(0.05,0.05)) +
  scale_y_continuous(name = 'Total impact', limits = c(4, 9), expand = c(0, 0)) +
  theme_black() +
  theme(legend.position = 'none')

p2 <- ggplot(inputs2 %>% filter(b1 < 0.75), aes(x = factor(s), y = impact, group = factor(b1), color = factor(b1))) +
  geom_line(size = 1) +
  geom_text(data = data.frame(s = c(2,2), impact = c(5, 5.8), b1 = factor(c(0.25, 0.5)), lab = c('Baseline~L[P] == 0.25', 'Baseline~L[P] == 0.5')), aes(label = lab), parse = TRUE, size = 5.5) +
  scale_x_discrete(name = 'Stage where FLW is reduced', labels = c('Producer', 'Retail', 'Consumer'), expand = c(0.05,0.05)) +
  scale_y_continuous(name = 'Total impact', limits = c(4, 9), expand = c(0, 0)) +
  theme_black() +
  theme(legend.position = 'none')

p3 <- ggplot(inputs2, aes(x = factor(s), y = impact, group = factor(b1), color = factor(b1))) +
  geom_line(size = 1) +
  geom_text(data = data.frame(s = c(2,2,2), impact = c(5, 5.8, 8.65), b1 = factor(c(0.25, 0.5, 0.75)), lab = c('Baseline~L[P] == 0.25', 'Baseline~L[P] == 0.5', 'Baseline~L[P] == 0.75')), aes(label = lab), parse = TRUE, size = 5.5) +
  scale_x_discrete(name = 'Stage where FLW is reduced', labels = c('Producer', 'Retail', 'Consumer'), expand = c(0.05,0.05)) +
  scale_y_continuous(name = 'Total impact', limits = c(4, 9), expand = c(0, 0)) +
  theme_black() +
  theme(legend.position = 'none')

ggsave('~/Dropbox/sesync/firstyear_talk/baselineimpact_plot_build1.png', p1 + scb, height = 4, width = 4, dpi = 300)
ggsave('~/Dropbox/sesync/firstyear_talk/baselineimpact_plot_build2.png', p2 + scb, height = 4, width = 4, dpi = 300)
ggsave('~/Dropbox/sesync/firstyear_talk/baselineimpact_plot_build3.png', p3 + scb, height = 4, width = 4, dpi = 300)


# Cost curves -------------------------------------------------------------


# Plot "diminishing returns" curve

curve(exp(-0.05*x), from = 0, to = 100)
curve(exp(-0.01*x), add = TRUE)

ggplot(data.frame(x = c(0, 100)), aes(x)) +
  stat_function(fun = function(x) exp(-0.05 * x), geom = 'line', size = 2, color = 'blue') +
  stat_function(fun = function(x) exp(-0.01 * x), geom = 'line', size = 2, color = 'red') +
  labs(x = 'Cost (billion $)', y = 'Food waste relative to baseline') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(0, 155), expand = c(0, 0)) +
  theme_bw() + theme(panel.grid = element_blank())

ggsave('~/Dropbox/sesync/firstyear_talk/cost_of_reduction_plot.png', height = 4, width = 4, dpi = 300)

# Large cost curve plot on black and white

ggplot(data.frame(x = c(0, 100)), aes(x)) +
  stat_function(fun = function(x) exp(-0.05 * x), geom = 'line', size = 2, color = 'blue') +
  stat_function(fun = function(x) exp(-0.01 * x), geom = 'line', size = 2, color = 'red') +
  labs(x = 'Cost (billion $)', y = 'Food waste relative to baseline') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(0, 155), expand = c(0, 0)) +
  theme_black() + theme(panel.grid = element_blank())

ggsave('~/Dropbox/sesync/firstyear_talk/cost_of_reduction_plot_BW.png', height = 4, width = 4, dpi = 300)

# show the two curves on separate plots.
# Shallow curve
ggplot(data.frame(x = c(0, 100)), aes(x)) +
  #stat_function(fun = function(x) exp(-0.05 * x), geom = 'line', size = 2, color = 'blue') +
  stat_function(fun = function(x) exp(-0.01 * x), geom = 'line', size = 2, color = 'red') +
  labs(x = 'Cost', y = 'Food waste') +
  scale_y_continuous(breaks = c(0, 1), labels = c('0%', '100%'), limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 155), expand = c(0, 0)) +
  theme_black() + theme(panel.grid = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.text.x = element_blank())
ggsave('~/Dropbox/sesync/firstyear_talk/cost_of_reduction_shallow.png', height = 3, width = 3, dpi = 300)


ggplot(data.frame(x = c(0, 100)), aes(x)) +
  stat_function(fun = function(x) exp(-0.05 * x), geom = 'line', size = 2, color = 'blue') +
  #stat_function(fun = function(x) exp(-0.01 * x), geom = 'line', size = 2, color = 'red') +
  labs(x = 'Cost', y = 'Food waste') +
  scale_y_continuous(breaks = c(0, 1), labels = c('0%', '100%'), limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 155), expand = c(0, 0)) +
  theme_black() + theme(panel.grid = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.text.x = element_blank())
ggsave('~/Dropbox/sesync/firstyear_talk/cost_of_reduction_steep.png', height = 3, width = 3, dpi = 300)


# Estimates of food waste magnitude ---------------------------------------

flwdat <- data.frame(Source = c('EPA\n(2016)', 'NRDC\n(2012)', 'ReFED\n(2016)', 'FAO\n(2011)', 'USDA\n(2014)'),
           Weight = 2.204 * c(111, 253, 176, 285, 188)) %>%
  mutate(Source = factor(Source, levels = Source[order(Weight)]))

ggplot(flwdat, aes(x = Source, y = Weight, fill = Source)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 640), name = 'Per capita food waste (lb)') +
  scale_fill_brewer(type = 'qual', palette = 'Set2') +
  theme_black() +
  theme(panel.grid = element_blank(),
        legend.position = 'none')
ggsave('~/Dropbox/sesync/firstyear_talk/flw_estimate_barchart.png', height = 4, width = 5, dpi = 300)

# Version for graphical abstract.
library(tidyverse)
flwdat <- data.frame(Source = c('EPA\n(2016)', 'NRDC\n(2012)', 'ReFED\n(2016)', 'FAO\n(2011)', 'USDA\n(2014)'),
                     Weight = 2.204 * c(111, 253, 176, 285, 188)) %>%
  mutate(Source = factor(Source, levels = Source[order(Weight)]))

p_abs <- ggplot(flwdat, aes(x = Source, y = Weight, fill = Source)) +
  geom_col() +
  geom_text(aes(label = Source), y = 50, color = 'white') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 640), name = 'Food waste\n(pounds per person per year)') +
  scale_fill_brewer(type = 'qual', palette = 'Set1') +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))
ggsave('~/google_drive/SESYNC Food Waste/Synthesis_MS/imgs for abstract/barchart_forabstract.png', p_abs, height = 3, width = 4, dpi = 300)

# Disposal to landfill.
disposaldat <- data.frame(destination = c('compost', 'bioenergy', 'landfill'), weight = c(2.1, 7.4, 30.3))
p_abs_donut <- ggplot(disposaldat, aes(x = 2, y = weight, fill = destination)) +
  geom_bar(stat = 'identity', color = 'white') +
  coord_polar(theta = 'y', start = 0) +
  xlim(1, 2.5) +
  scale_fill_brewer(type = 'qual', palette = 'Set1') +
  theme_void() +
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 16))
ggsave('~/google_drive/SESYNC Food Waste/Synthesis_MS/imgs for abstract/donutchart_forabstract.png', p_abs_donut, height = 3, width = 4, dpi = 300)

# RGB
hexcodes <- RColorBrewer::brewer.pal(5, 'Set1')
col2rgb(hexcodes)
