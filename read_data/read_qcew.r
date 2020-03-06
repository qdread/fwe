# Read Bureau of Labor Statistics QCEW (Qtrly census of employment and wages) data
# Only need US-wide 2012 annual averages for establishments of all sizes

library(tidyverse)
library(data.table)

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
qcew12 <- fread(file.path(fp, 'Census/QCEW/2012.annual.singlefile.csv'))

# Size code is not included
qcew_us <- qcew12 %>%
  filter(area_fips %in% 'US000') %>%
  select(own_code, industry_code, agglvl_code, annual_avg_estabs, annual_avg_emplvl)

qcew_us_ag <- qcew_us %>%
  filter(grepl('^11*', industry_code))

qcew_us_othercrop <- qcew_us %>%
  filter(grepl('^1119', industry_code))

# 9469 "other crop": considering privately owned farms only
# not food: 456 tobacco, 3246 cotton, 1562 hay
# food: 293 sugarcane, 306 sugarbeet, 174 peanut
# mixture: 3433 other or combination crops. This includes grass seed farms. Assuming they are all equal, we get 12/28
# 12/28 * 3433 = 1471

(293 + 306 + 174 + 1471) / 9469
# 23.7% of this is food crops.

qcew_us_greenhouse <- qcew_us %>%
  filter(grepl('^1114', industry_code))

# 11141 is food, 11142 is not food.
681/8440
# 8.07% of this is food crops.


# Seafood proportions (added 06 mar 2020) ---------------------------------

# 112 for animal farms and aquaculture
# 1121xx is cattle, 1123xx is chickens, 112xxx (anything else) is the one that may contain fish, corresponding to 112A00.

animal_production <- qcew_us %>% 
  filter(grepl('^112', industry_code), nchar(industry_code) >= 4) %>%
  mutate(animal = case_when(
    grepl('^1121', industry_code) ~ 'cattle',
    grepl('^1123', industry_code) ~ 'poultry',
    TRUE ~ 'other'
  )) %>%
  group_by(animal, industry_code) %>%
  summarize(estabs = sum(annual_avg_estabs), emps = sum(annual_avg_emplvl))

# Within "other", the proportion that is 1125xx is the one we use.

animal_production %>%
  filter(animal == 'other', nchar(industry_code) == 4) %>%
  mutate_if(is.numeric, ~ ./sum(.))

# The result is that 11.3% of code 112A00 is fish or shellfish aquaculture.

# 114 is wild fish and game, where 114111 and 114112 (but not 114119) is fish, 1142 is other stuff

wild_fish_game <- qcew_us %>% 
  filter(grepl('^114', industry_code)) %>%
  group_by(industry_code) %>%
  summarize(estabs = sum(annual_avg_estabs), emps = sum(annual_avg_emplvl))

wild_fish_game[4:7,]

# proportion by employees
sum(wild_fish_game$emps[4:5])/sum(wild_fish_game$emps[4:7])
# 75.7% of code 114000 is fish or shellfish wild catching.