# Schematic for hypothesis document

library(ggplot2)

dat <- data.frame(intercept = c(-25, -125, -225), slope = c(7,6,5), lab = c('5% FLW reduction', '25% FLW reduction', '45% FLW reduction'))

p1 <- ggplot(dat) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = 'gray85') +
  geom_abline(aes(slope=slope, intercept=intercept, color=lab), size = 1) +
  geom_hline(yintercept = 0, linetype = 'dotted', size = 1.5) +
  geom_text(x = Inf, y = -Inf, hjust = 1.2, vjust = -1.2, label = 'Net decrease') +
  geom_text(x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2, label = 'Net increase') +
  geom_text(aes(x = 25, y = slope * 25 + intercept + 20, label = lab, color = lab), angle = atan(dat$slope * 0.1) * 180/pi) +
  scale_x_continuous(name = 'Percent increase in packaging demand', limits = c(0, 50)) +
  scale_y_continuous(name = 'GHG emissions relative to baseline', limits = c(-250, 250), breaks = seq(-200,200,by=100), labels = c('-200','-100','0','+100','+200')) +
  coord_fixed(ratio = 0.1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = 'none')

ggsave('~/Dropbox/projects/foodwaste/Results/schematic1.png', height = 3.5, width = 3.5, dpi = 300)
