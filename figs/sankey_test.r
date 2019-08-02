# Attempt to create a schematic Sankey diagram.
# See https://www.r-bloggers.com/how-to-create-sankey-diagrams-from-tables-data-frames-using-r/

library(flipPlots)
library(tidyverse)

sankeydat <- read.csv('~/Documents/R/sankeydat.csv')

sankeydat$substage[sankeydat$stage != 'consumption'] <- NA

sankeylong <- sankeydat %>% gather(type, value, -stage, -substage) %>% mutate(stage = factor(stage, levels = c('production','processing','retail','consumption')))

SankeyDiagram(data = sankeylong[,-4], weights = sankeylong$value, label.show.varname = FALSE)

sankeydatv2 <- read.csv('~/Documents/R/sankeydatv2.csv')
SankeyDiagram(data = sankeydatv2 %>% select(-value) %>% mutate(stage4 = gsub('consumption: ', '', stage4)), weights = sankeydatv2 %>% pull(value), label.show.varname = FALSE, font.family = 'Arial', font.size = 36, node.padding = 30, link.color = 'Target')

sankeydatv3 <- read.csv('~/Documents/R/sankeydatv3.csv')
SankeyDiagram(data = sankeydatv3 %>% select(-value) %>% mutate(stage4 = gsub('consumption: ', '', stage4)), weights = sankeydatv3 %>% pull(value), label.show.varname = FALSE, font.family = 'Arial', font.size = 36, node.padding = 30, link.color = 'Target')

sankeydatv4 <- read.csv('~/Documents/R/sankeydatv4.csv')
with(sankeydatv4,
  SankeyDiagram(data = data.frame(from, to), weights = value, label.show.varname = FALSE, font.family = 'Arial', font.size = 36, node.padding = 30, link.color = 'Target')
)
