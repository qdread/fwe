# Check NLCD against each other

nlcdold <- read.csv('Q:/NLCD/corrected_nlcd_bcr_old.csv')
nlcd11 <- read.csv('Q:/NLCD/corrected_nlcd_bcr_11.csv')

library(dplyr)

old_dict <- data.frame(l1 = 1:9, type = c('developed', 'ag', 'natural', 'natural', 'water', 'natural', 'natural', 'natural', 'water'))
new_dict <- data.frame(l1 = 1:9, type = c('water', 'developed', 'natural', 'natural', 'natural', NA, 'natural', 'ag', 'natural'))


sum_old <- nlcdold %>% 
  mutate(l1 = floor(category/10)) %>%
  left_join(old_dict) %>%
  group_by(type) %>% 
  summarize(total_old = sum(as.numeric(Freq)))
sum_new <- nlcd11 %>% 
  mutate(l1 = floor(category/10)) %>%
  left_join(new_dict) %>%
  group_by(type) %>% 
  summarize(total_new = sum(as.numeric(Freq)))

# Looks like the two can't really be compared so this is more or less a dead end.