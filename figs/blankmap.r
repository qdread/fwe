library(urbnmapr)

blankmap <-get_urbn_map()

ggplot(blankmap, aes(x=long,y=lat,group=group)) + 
  geom_path(color = 'gray75') +
  theme_black() +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
ggsave('~/Dropbox/sesync/firstyear_talk/usamap.png', height = 6, width = 9, dpi = 300)
