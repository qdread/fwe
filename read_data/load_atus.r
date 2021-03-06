# American Time Use Survey datasets
# Data from 2003-2016

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')

library(atus)
library(tidyverse)

data(atusact) # Observations: respondent ID, activity tier code, duration
data(atuscps) # Links respondent ID with current population survey info for that individual (sex, age, educ, race, etc)
data(atusresp) # Same rows as atuscps, more information about the respondents, including what days they recorded the diary

# Load lookup table of major categories
atusdict <- read.csv(file.path(fp, 'ATUS/atus_dict.csv'), colClasses = rep('character',2)) 

# Codes related to food
household_food_codes <- c(food_prep = "020201",
                          food_presentation = "020202",
                          food_cleanup = "020203",
                          food_notspecified = "020299")
food_shopping_codes <- c(grocery_shopping = "070101",
                         nongrocery_food_shopping = "070103",
                         shopping_waiting = "070105",
                         shopping_notspecified = "070199")
eating_drinking_codes <- c(eating_drinking = "110101",
                           eating_drinking_nonspecified = "110199",
                           eating_drinking_waiting = "110281",
                           eating_drinking_waiting_nonspecified = "110289",
                           eating_drinking_nonspecifiedtwice = "119999",
                           eating_drinking_atjob = "050202")
misc_food_codes <- c(volunteering_food_prep = "150201",
                     using_meal_prep_service = "090102")
travel_food_codes <- c(grocery_shopping_travel = "180701",
                       eating_drinking_travel = "181101",
                       eating_drinking_travel_nonspecified = "181199")
all_foodcodes <- map2_dfr(list(household_food_codes, food_shopping_codes, eating_drinking_codes, misc_food_codes, travel_food_codes),
                          c('household food prep and cleanup', 'grocery shopping', 'eating and drinking', 'misc food prep', 'travel for grocery shopping or eating'),
                          ~ (data.frame(tiercode = .x, food_activity_name = names(.x), food_activity_group = .y)))
  

# Create visualizations of raw data and weighted averages

# Add zeroes to df
atusact_with0 <- atusact %>%
  mutate(tiercode = sprintf('%06d', tiercode)) %>%
  spread(tiercode, dur, fill = 0) %>%
  gather(tiercode, dur, -tucaseid)

atusact_classified <- atusact_with0 %>%
  ungroup %>%
  left_join(all_foodcodes) %>% 
  mutate(major_category = atusdict$major_category[match(substr(tiercode, 1, 2), atusdict$code)],
         food_activity_name = fct_explicit_na(food_activity_name, 'nonfood'),
         food_activity_group = fct_explicit_na(food_activity_group, 'nonfood')) 
  
# Abbreviated version that sums up anything not potentially related to food
atusact_sums <- atusact_classified %>%
  mutate(tucaseid = as.character(tucaseid)) %>%
  group_by(tucaseid, major_category, food_activity_group, food_activity_name) %>%
  summarize(dur = sum(dur))

write.csv(atusact_sums, file.path(fp, 'ATUS/final_data/atus_sums_with0s.csv'), row.names = FALSE)

# Older code --------------------------------------------------------------

# Check total durations
totals <- atusact_classified %>% summarize(dur=sum(dur))
all(totals$dur == 1440)
table(totals$dur == 1440)
table(totals$dur > 1380) # Most account for at least 23 hours.

# proportion duration (lumping all non-food together)
atusact_summ <- atusact_classified %>%
  group_by(tucaseid, food_activity_group, food_activity_name) %>%
  summarize(dur = sum(dur)) 

atusact_summ <- atusact_summ %>%
  group_by(tucaseid) %>%
  mutate(dur_prop = dur/sum(dur))

# Get a bunch of quantiles.
# Most are zero so this won't really work
qs <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
atusact_quant <- atusact_summ %>%
  ungroup %>%
  group_by(food_activity_group, food_activity_name) %>%
  do(setNames(as.data.frame(t(quantile(.$dur, qs))), paste0('q', qs)))

atusact_sums <- atusact_summ %>%
  ungroup %>%
  group_by(food_activity_group, food_activity_name) %>%
  summarize(time = sum(dur))

atusact_quant_plot <- atusact_quant %>% 
  filter(!food_activity_group %in% 'nonfood', !grepl('nonspecified|notspecified', food_activity_name)) %>%
  arrange(q0.5) 
ggplot(atusact_quant_plot) +
  geom_pointrange(aes(x = 1:nrow(atusact_summ_plot), color = food_activity_group, y = q0.5, ymin = q0.025, ymax = q0.975)) +
  scale_x_continuous(breaks = 1:nrow(atusact_summ_plot), labels = atusact_summ_plot$food_activity_name, name = 'Food-related activity') +
  scale_y_continuous(name = 'Minutes spent') +
  coord_flip() +
  theme_bw()

atusact_sums_plot <- atusact_sums %>% 
  filter(!food_activity_group %in% 'nonfood', !grepl('nonspecified|notspecified', food_activity_name)) %>%
  arrange(time) 
ggplot(atusact_sums_plot) +
  geom_point(aes(x = 1:nrow(atusact_sums_plot), color = food_activity_group, y = time), size = 3) +
  scale_x_continuous(breaks = 1:nrow(atusact_sums_plot), labels = atusact_sums_plot$food_activity_name, name = 'Food-related activity') +
  scale_y_continuous(name = 'Minutes spent') +
  coord_flip() +
  theme_bw() +
  theme(legend.position = 'bottom', legend.text = element_text(size = 7)) +
  guides(color = guide_legend(nrow=3,byrow=TRUE, title = NULL))

ggsave('~/Dropbox/projects/foodwaste/Results/food_timespent.png', height = 6, width = 6, dpi = 300)



