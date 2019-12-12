# Schematic figure showing relationship of input and output

ggplot(data.frame(x=c(0,1),y=c(0,1))) +
  geom_abline(slope=1, size=1, color='goldenrod') +
  geom_abline(slope=2, size=1, color='forestgreen') +
  geom_segment(x = 0.5, xend = 0.5, y = 0, yend = 0.5, linetype = 'dashed') +
  geom_segment(x = 0.25, xend = 0.25, y = 0, yend = 0.5, linetype = 'dashed') +
  geom_segment(x = 0, xend = 0.5, y = 0.5, yend = 0.5, linetype = 'dashed') +
  geom_point(data = data.frame(x = c(0,0.25,0.25,0.5,0.5), y = c(0.5, 0.5, 0, 0.5, 0)), aes(x=x, y=y), size = 2) +
  scale_x_continuous(name = 'Input', expand=c(0,0), limits=c(0,0.6)) +
  scale_y_continuous(name = 'Output', expand=c(0,0), limits=c(0,0.6)) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

ggsave('Q:/figures/stoten_ms/schematicfiginputoutput.png', height=4, width=4, dpi=300)
