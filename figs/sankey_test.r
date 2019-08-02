# Attempt to create a schematic Sankey diagram.
# See https://www.r-bloggers.com/how-to-create-sankey-diagrams-from-tables-data-frames-using-r/

library(flipPlots)
library(tidyverse)

sankeydat <- read.csv('~/Documents/R/sankeydat.csv')

sankeydat$substage[sankeydat$stage != 'consumption'] <- NA

sankeylong <- sankeydat %>% gather(type, value, -stage, -substage) %>% mutate(stage = factor(stage, levels = c('production','processing','retail','consumption')))

SankeyDiagram(data = sankeylong[,-4], weights = sankeylong$value, label.show.varname = FALSE)

sankeydatv2 <- read.csv('~/Documents/R/sankeydatv2.csv')
SankeyDiagram(data = sankeydatv2 %>% select(-value) %>% mutate(stage4 = gsub('consumption: ', '', stage4)), weights = sankeydatv2 %>% pull(value), label.show.varname = FALSE, font.family = 'Arial', font.size = 36, node.padding = 60, link.color = 'None', colors = RColorBrewer::brewer.pal(8,'Set1'))

sankeydatv3 <- read.csv('~/Documents/R/sankeydatv3.csv')
SankeyDiagram(data = sankeydatv3 %>% select(-value) %>% mutate(stage4 = gsub('consumption: ', '', stage4)), weights = sankeydatv3 %>% pull(value), label.show.varname = FALSE, font.family = 'Arial', font.size = 36, node.padding = 30, link.color = 'Target')

sankeydatv4 <- read.csv('~/Documents/R/sankeydatv4.csv')
with(sankeydatv4,
  SankeyDiagram(data = data.frame(from, to), weights = value, label.show.varname = FALSE, font.family = 'Arial', font.size = 36, node.padding = 30, link.color = 'Target')
)


# Use networkd3 -----------------------------------------------------------

# Source: https://www.r-graph-gallery.com/323-sankey-diagram-with-the-networkd3-library/

library(networkD3)

slinks <- read.csv('~/Documents/R/sankey_links.csv')
snodes <- read.csv('~/Documents/R/sankey_nodes.csv')

colors <- RColorBrewer::brewer.pal(5, 'Set2')
mycolorscale <- 'd3.scaleOrdinal() .domain(["level1", "level2", "level3", "level4", "final"]) .range(["#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"])'

sankeyNetwork(Links = slinks, Nodes = snodes, Source = "from",
              Target = "to", Value = "value", NodeID = "name", NodeGroup = "group",
              units = "none", colourScale = mycolorscale,
              fontSize = 20, fontFamily = 'Arial', nodeWidth = 20, nodePadding = 150)
