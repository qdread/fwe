# Look at FAO data for all regions (continents)

faoall <- read.csv('~/Dropbox/projects/foodwaste/Data/fao_allregions.csv', stringsAsFactors = FALSE, dec = ',')

library(tidyverse)
faoall %>%
  gather(stage, value, -Continent, -Food) %>%
  mutate(Food = trimws(Food)) %>%
  filter(Food == 'Total') %>%
  group_by(stage) %>%
  summarize(value = sum(value))
