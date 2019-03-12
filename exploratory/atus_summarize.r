# View and summarize some data from ATUS
# QDR / FWE / 11 Mar 2019

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')

library(atus)
library(tidyverse)

data(atuscps) # Respondent data
atus_sums <- read.csv(file.path(fp, 'ATUS/final_data/atus_sums_with0s.csv'), colClasses = c('character','character','character','character','integer'))

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

# Division of food-related activities
# Need to add zeroes back in.

atus_sums_agg <- atus_sums %>%
  group_by(tucaseid, major_category, food_activity_group) %>%
  summarize(dur = sum(dur)) %>%
  group_by(major_category, food_activity_group) %>%
  summarize(dur = mean(dur))

atus_sums_agg %>% arrange(food_activity_group, major_category) %>% print(n=nrow(.))

# Important number
atus_sums_agg %>% filter(major_category == 'traveling')
11.3/(11.3+60.8) #16% of travel time is grocery shopping or eating related.

ggplot(atus_sums_agg %>% filter(!food_activity_group %in% 'nonfood')) +
  geom_col(aes(x = 1, y = dur, fill = food_activity_group), position = 'stack') +
  scale_x_continuous(limits = c(0.3, 1.7)) +
  scale_y_continuous(name = "Time (min)", limits = c(0, 130), expand = c(0,0)) +
  theme_bw() +
  ggtitle('How are the 2 hours spent on food divided?', 'source: American Time Use Survey') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

# Distinguish the different types of travel
travel_agg <- atus_sums %>%
  filter(major_category %in% 'traveling') %>%
  group_by(tucaseid, food_activity_group, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  group_by(food_activity_group, food_activity_name) %>%
  summarize(dur = mean(dur))

travel_agg %>% ungroup %>% mutate(prop = dur/sum(dur))
# 11% travel for eating at restaurants, 6% travel for grocery shopping

# The average American spends 8 min/day traveling to eat and 4 min/day traveling to grocery shopping.


# Weighted average
atuswts <- atusresp %>%
  mutate(tucaseid = as.character(tucaseid)) %>%
  select(tucaseid, wt)

atus_sums_weighted <- atus_sums %>%
  group_by(tucaseid, major_category, food_activity_group, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  ungroup %>%
  left_join(atuswts) %>%
  group_by(major_category, food_activity_group, food_activity_name) %>%
  summarize(dur = weighted.mean(dur, wt))

atus_sums_weighted %>% arrange(major_category) %>% print(n=nrow(.))
atus_sums_weighted %>% filter(major_category %in% 'traveling')


# Demographic differences -------------------------------------------------

# We have region, state, sex, age, education, race, marital status, home type, family income, and other more detailed things

tabulate_atus <- function(data, variable) {
  quo_var <- enquo(variable)
  wts <- atuscps %>%
    left_join(atusresp %>% select(tucaseid, tuyear, wt)) %>%
    select(tucaseid, wt, !!quo_var) %>%
    mutate(tucaseid = as.character(tucaseid))
  
  data %>%
    group_by(tucaseid, major_category, food_activity_group, food_activity_name) %>%
    summarize(dur = sum(dur)) %>%
    ungroup %>%
    left_join(wts) %>%
    group_by(!!quo_var, major_category, food_activity_group, food_activity_name) %>%
    summarize(dur = weighted.mean(dur, wt))
}

atus_sums_weighted_region <- atus_sums %>% tabulate_atus(region)
atus_sums_weighted_year <- atus_sums %>% tabulate_atus(tuyear)
atuscps$age_cat <- cut(atuscps$age, breaks = seq(10,90,10))
atus_sums_weighted_age <- atus_sums %>% tabulate_atus(age_cat)
atus_sums_weighted_sex <- atus_sums %>% tabulate_atus(sex)
atus_sums_weighted_education <- atus_sums %>% tabulate_atus(edu)
atus_sums_weighted_race <- atus_sums %>% tabulate_atus(race)
atus_sums_weighted_famincome <- atus_sums %>% tabulate_atus(famincome)

save(list = ls(pattern = 'atus_sums_agg|atus_sums_weighted'), file = file.path(fp, 'ATUS/final_data/sums_by_group.RData'))


# Plot by demographic group -----------------------------------------------

load(file.path(fp, 'ATUS/final_data/sums_by_group.RData'))

# Overall - major categories
atus_sums_weighted %>%
  group_by(major_category) %>%
  summarize(dur = sum(dur)) %>%
ggplot() +
  geom_col(aes(x = 1, y = dur, fill = major_category), position = 'stack') +
  scale_x_continuous(limits = c(0.3, 1.7)) +
  scale_y_continuous(name = "Time (min)", limits = c(0, 1440), expand = c(0,0)) +
  theme_bw() +
  coord_flip() +
  ggtitle('How the average American\'s day is spent', 'source: American Time Use Survey') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

# Make more of a table with this.
write.csv(atus_sums_weighted, file = '~/google_drive/SESYNC Food Waste/Results_Figures/atus_sums.csv', row.names = FALSE)

# Important number
atus_sums_weighted %>% filter(major_category == 'traveling')

ggplot(atus_sums_weighted %>% filter(!food_activity_group %in% 'nonfood')) +
  geom_col(aes(x = 1, y = dur, fill = food_activity_group), position = 'stack') +
  scale_x_continuous(limits = c(0.3, 1.7)) +
  scale_y_continuous(name = "Time (min)", limits = c(0, 130), expand = c(0,0)) +
  scale_fill_discrete(name = 'Activity') +
  theme_bw() +
  ggtitle('How are the 2 hours spent on food divided?', 'source: American Time Use Survey') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
ggsave('~/google_drive/SESYNC Food Waste/Results_Figures/atus_foodtime.png', height = 5, width = 6)

# Distinguish the different types of travel
atus_sums_weighted %>% filter(major_category == 'traveling') %>% mutate(prop = dur/sum(dur))
# 9.6% travel for eating at restaurants, 4.5% travel for grocery shopping.

# Plot over time
brewfill <- scale_fill_brewer(type = 'qual', palette = 'Set1', name = 'Activity', labels = c('Travel to eat and drink outside the home', 'Travel to grocery shopping', 'Other'))

yearstacked <- atus_sums_weighted_year %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(tuyear, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  ggplot() +
    geom_col(aes(x = tuyear, y = dur, fill = food_activity_name), position = 'stack') +
    scale_x_continuous(name = 'Year', labels = 2003:2016, breaks = 2003:2016) +
    scale_y_continuous(name = "Time (min)", limits = c(0, 80), expand = c(0,0)) +
    brewfill + 
    theme_bw() +
    theme(legend.position = 'bottom') +
    ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

sexstacked <- atus_sums_weighted_sex %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(sex, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  ggplot() +
  geom_col(aes(x = sex, y = dur, fill = food_activity_name), position = 'stack') +
  scale_y_continuous(name = "Time (min)", limits = c(0, 80), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

racestacked <- atus_sums_weighted_race %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(race, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  ggplot() +
  geom_col(aes(x = race, y = dur, fill = food_activity_name), position = 'stack') +
  scale_y_continuous(name = "Time (min)", limits = c(0, 80), expand = c(0,0)) +
  scale_fill_brewer(type = 'qual', palette = 'Set1') +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

agestacked <- atus_sums_weighted_age %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(age_cat, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  ggplot() +
  geom_col(aes(x = age_cat, y = dur, fill = food_activity_name), position = 'stack') +
  scale_x_discrete(name = 'Age Group', labels = c('15-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')) +
  scale_y_continuous(name = "Time (min)", limits = c(0, 80), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

incomestacked <- atus_sums_weighted_famincome %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified', !is.na(famincome)) %>%
  group_by(famincome, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  ggplot() +
  geom_col(aes(x = famincome, y = dur, fill = food_activity_name), position = 'stack') +
  scale_x_discrete(name = 'Household income') +
  scale_y_continuous(name = "Time (min)", limits = c(0, 90), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

regionstacked <- atus_sums_weighted_region %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(region, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  ggplot() +
  geom_col(aes(x = region, y = dur, fill = food_activity_name), position = 'stack') +
  scale_y_continuous(name = "Time (min)", limits = c(0, 80), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

# Same graphs as percentages ----------------------------------------------

yearpct <- atus_sums_weighted_year %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(tuyear, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  group_by(tuyear) %>%
  mutate(prop = dur/sum(dur)) %>%
  filter(!food_activity_name %in% 'nonfood') %>%
  ggplot() +
  geom_col(aes(x = tuyear, y = prop, fill = food_activity_name), position = 'dodge') +
  scale_x_continuous(name = 'Year', labels = 2003:2016, breaks = 2003:2016) +
  scale_y_continuous(name = "Proportion", limits = c(0, 0.11), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

sexpct <- atus_sums_weighted_sex %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(sex, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  group_by(sex) %>%
  mutate(prop = dur/sum(dur)) %>%
  filter(!food_activity_name %in% 'nonfood') %>%
  ggplot() +
  geom_col(aes(x = sex, y = prop, fill = food_activity_name), position = 'dodge') +
  scale_y_continuous(name = "Proportion", limits = c(0, 0.11), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

racepct <- atus_sums_weighted_race %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(race, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  group_by(race) %>%
  mutate(prop = dur/sum(dur)) %>%
  filter(!food_activity_name %in% 'nonfood') %>%
  ggplot() +
  geom_col(aes(x = race, y = prop, fill = food_activity_name), position = 'dodge') +
  scale_y_continuous(name = "Proportion", limits = c(0, 0.11), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

agepct <- atus_sums_weighted_age %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(age_cat, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  group_by(age_cat) %>%
  mutate(prop = dur/sum(dur)) %>%
  filter(!food_activity_name %in% 'nonfood') %>%
  ggplot() +
  geom_col(aes(x = age_cat, y = prop, fill = food_activity_name), position = 'dodge') +
  scale_x_discrete(name = 'Age Group', labels = c('15-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')) +
  scale_y_continuous(name = "Proportion", limits = c(0, 0.2), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

incomepct <- atus_sums_weighted_famincome %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified', !is.na(famincome)) %>%
  group_by(famincome, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  group_by(famincome) %>%
  mutate(prop = dur/sum(dur)) %>%
  filter(!food_activity_name %in% 'nonfood') %>%
  ggplot() +
  geom_col(aes(x = famincome, y = prop, fill = food_activity_name), position = 'dodge') +
  scale_x_discrete(name = 'Household income') +
  scale_y_continuous(name = "Proportion", limits = c(0, 0.12), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

regionpct <- atus_sums_weighted_region %>% 
  filter(major_category %in% 'traveling', !food_activity_name %in% 'eating_drinking_travel_nonspecified') %>%
  group_by(region, food_activity_name) %>%
  summarize(dur = sum(dur)) %>%
  group_by(region) %>%
  mutate(prop = dur/sum(dur)) %>%
  filter(!food_activity_name %in% 'nonfood') %>%
  ggplot() +
  geom_col(aes(x = region, y = prop, fill = food_activity_name), position = 'dodge') +
  scale_y_continuous(name = "Proportion", limits = c(0, 0.11), expand = c(0,0)) +
  brewfill + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle('Proportion of travel related to food', 'source: American Time Use Survey') 

pdf('~/google_drive/SESYNC Food Waste/Results_Figures/atus_stackedbarplots.pdf', height = 5, width = 7)
yearstacked; sexstacked; agestacked; racestacked; incomestacked; regionstacked
dev.off()

pdf('~/google_drive/SESYNC Food Waste/Results_Figures/atus_proportionbarplots.pdf', height = 5, width = 7)
yearpct; sexpct; agepct; racepct; incomepct; regionpct
dev.off()