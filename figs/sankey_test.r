# Attempt to create a schematic Sankey diagram.
# See https://www.r-bloggers.com/how-to-create-sankey-diagrams-from-tables-data-frames-using-r/

library(flipPlots)
library(tidyverse)

sankeydat <- read.csv('~/R/sankeydat.csv')

sankeydat$substage[sankeydat$stage != 'consumption'] <- NA

sankeylong <- sankeydat %>% gather(type, value, -stage, -substage) %>% mutate(stage = factor(stage, levels = c('production','processing','retail','consumption')))

SankeyDiagram(data = sankeylong[,-4], weights = sankeylong$value, label.show.varname = FALSE)

sankeydatv2 <- read.csv('~/R/sankeydatv2.csv')
SankeyDiagram(data = sankeydatv2 %>% select(-value) %>% mutate(stage4 = gsub('consumption: ', '', stage4)), weights = sankeydatv2 %>% pull(value), label.show.varname = FALSE, font.family = 'Arial', font.size = 36, node.padding = 30)

# The below code doesn't work. Might have to use point and click export.
png('Q:/figures/sankey1.png', height = 6, width = 8, res = 300, units = 'in')
SankeyDiagram(data = sankeydatv2 %>% select(-value), weights = sankeydatv2 %>% pull(value), label.show.varname = FALSE, font.family = 'Arial')
dev.off()
