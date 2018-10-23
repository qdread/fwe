# Make a map of BCR boundaries

library(rgdal)
bcr <- readOGR(dsn = 'Q:/ecoregions', layer = 'bcr_usa_combined')
states <- read.csv('Q:/states_albers.csv')

library(ggplot2)
library(RColorBrewer)

bcrfort <- fortify(bcr, region = 'BCR')
repcolors <- rep_len(RColorBrewer::brewer.pal(5, 'Set1'), 32)

ggplot() +
  geom_polygon(data = bcrfort, aes(x=long, y=lat, group=group, fill=factor(id)), alpha = 0.75) +
  geom_path(data = states, aes(x=long, y=lat, group=group)) +
  scale_fill_manual(values = repcolors)
  

nacol <- function(spdf){
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  nunique <- function(x){unique(x[!is.na(x)])}
  np = nrow(spdf)
  adjl = spdep::poly2nb(spdf)
  cols = rep(NA, np)
  cols[1]=1
  nextColour = 2
  
  for(k in 2:np){
    adjcolours = nunique(cols[adjl[[k]]])
    if(length(adjcolours)==0){
      cols[k]=resample(cols[!is.na(cols)],1)
    }else{
      avail = setdiff(nunique(cols), nunique(adjcolours))
      if(length(avail)==0){
        cols[k]=nextColour
        nextColour=nextColour+1
      }else{
        cols[k]=resample(avail,size=1)
      }
    }
  }
  return(cols)
}

set.seed(7777)
(bcrcol <- nacol(bcr))

lightcols <- RColorBrewer::brewer.pal(5, 'Pastel1')

library(dplyr)
bcrfort <- left_join(bcrfort, data.frame(id = as.character(bcr@data$BCR), fillcol = bcrcol))

ggplot() +
  geom_polygon(data = bcrfort, aes(x=long, y=lat, group=group, fill=factor(fillcol))) +
  geom_path(data = states, aes(x=long, y=lat, group=group)) +
  scale_fill_manual(values = lightcols) +
  theme_minimal() +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'black'))
ggsave('~/Dropbox/projects/foodwaste/Meetings/bcrmap.png', height=6, width=9)
