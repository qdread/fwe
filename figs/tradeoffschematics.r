# Plot equations to show reduction benefits

library(tidyverse)

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
