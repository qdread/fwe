# View and summarize some data from ATUS
# QDR / FWE / 11 Mar 2019

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')

library(atus)
library(tidyverse)

data(atuscps) # Respondent data
atus_sums <- read.csv(file.path(fp, 'ATUS/final_data/atus_sums.csv'), colClasses = c('character','character','character','character','integer'))

# Add rows if there are unaccounted minutes
extratime <- atus_sums %>% 
  group_by(tucaseid) %>%
  summarize(unaccounted = 1440 - sum(dur)) 

# Get rid of the individuals with less than 20 hours accounted for
bad_ids <- extratime$tucaseid[extratime$unaccounted > 240]

atus_sums <- atus_sums %>%
  filter(!tucaseid %in% bad_ids)

# Add rows for the unaccounted time
extratime <- extratime %>%
  filter(unaccounted > 0, !tucaseid %in% bad_ids)

extratime <- data.frame(tucaseid = extratime$tucaseid, major_category = 'unaccounted time', food_activity_group = 'nonfood', food_activity_name = 'nonfood', dur = extratime$unaccounted, stringsAsFactors = FALSE)

atus_sums <- bind_rows(atus_sums, extratime)

# Just look at time spent doing things related to food

# Sick ass pie charts

food_time_byindiv <- atus_sums %>%
  mutate(is_food = !food_activity_group %in% 'nonfood') %>%
  group_by(tucaseid, is_food) %>%
  summarize(dur = sum(dur))

food_time_byindiv %>% group_by(is_food) %>% summarize(dur = mean(dur)) %>% ggplot() +
  geom_col(aes(x = 1, y = dur, fill = is_food), position = 'stack') +
  coord_polar(theta = 'y') +
  theme_void() +
  annotate('text', x = 1, y = c(60, 720), label = c('2:07', '21:53')) +
  scale_fill_discrete(name = 'Food-related?', labels = c('no','yes')) +
  ggtitle('The average American spends about 2 hours a day on food-related activities', 'source: American Time Use Survey')

