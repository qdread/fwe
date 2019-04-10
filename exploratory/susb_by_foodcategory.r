# Use economic census data to find the proportion of the "partial" food system sectors that are attributed to the food system.

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fpcrosswalk <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')

cs <- c('factor','numeric','character')
susb12 <- read.csv(file.path(fp, 'Census/SUSB/us_state_6digitnaics_2012.txt'), colClasses = cs[c(1,1,1,2,2,2,1,1,2,1,2,1,3,3,3)]) 

# Read NAICS codes
naics12 <- read.csv(file.path(fpcrosswalk, '2012naics_foodclassified.csv'), stringsAsFactors = FALSE)

# Which ones are the partial food system sectors
naics12_food <- naics12 %>% filter(!eeio_code %in% '') %>% select(1:5)

naics12_foodcodes <- naics12_food %>% 
  group_by(eeio_code, NAICS12) %>%
  summarize(n_food = sum(is_food %in% c('y','partial')), n_nonfood = sum(!is_food %in% c('y','partial')))

# The ones with any food rows
codes_to_match <- naics12_foodcodes %>% filter(n_food > 0) %>% pull(NAICS12)

susb12US <- susb12 %>% filter(STATEDSCR == 'United States', ENTRSIZEDSCR == 'Total')
codes_to_match %in% susb12US$NAICS

# All codes
naics12_foodcodes$NAICS12 %in% susb12US$NAICS # All are there

# Proportion by total receipts
naics12_foodcodes %>% 
  rename(NAICS = NAICS12) %>%
  left_join(susb12US) %>%
  mutate(any_food = n_food > 0) %>%
  group_by(eeio_code, any_food) %>%
  summarize(total_receipts = sum(RCPT_N))

naics12_foodcodes %>% 
  rename(NAICS = NAICS12) %>%
  left_join(susb12US) %>%
  mutate(any_food = n_food > 0) %>%
  group_by(eeio_code) %>%
  summarize(proportion_foodsystem = sum(RCPT_N[any_food])/sum(RCPT_N))

# Proportion by establishment
naics12_foodcodes %>% 
  rename(NAICS = NAICS12) %>%
  left_join(susb12US) %>%
  mutate(any_food = n_food > 0) %>%
  group_by(eeio_code) %>%
  summarize(establishments_food = sum(ESTB[any_food]),
            establishments_total = sum(ESTB),
            proportion_estb_food = establishments_food/establishments_total)

# Proportion by establishment, with proportion within row as well (for codes that have some food and some not)
naics12_foodcodes %>% 
  rename(NAICS = NAICS12) %>%
  left_join(susb12US) %>%
  mutate(any_food = n_food > 0,
         estb_food = ESTB * n_food/(n_food+n_nonfood),
         estb_nonfood = ESTB - estb_food) %>%
  #filter(eeio_code=='445000') %>% select(eeio_code,NAICS,n_food,n_nonfood,ESTB,estb_food,estb_nonfood, NAICSDSCR)
  group_by(eeio_code) %>%
  summarize(establishments_food = sum(estb_food),
            establishments_total = sum(estb_food) + sum(estb_nonfood),
            proportion_estb_food = establishments_food/establishments_total)

# Check 445000 (food and beverage stores)
naics12_foodcodes %>% 
  rename(NAICS = NAICS12) %>%
  left_join(susb12US) %>%
  filter(eeio_code == '445000')


