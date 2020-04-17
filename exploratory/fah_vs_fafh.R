# Preliminary analysis: environmental impacts of food at home versus food away from home, using I-O model or similar.
# QDR / 16 April 2020

# The goal is to quickly take a look at per dollar and per serving environmental impacts of FAH and FAFH
# Research question: what would the consequences be of switching a lot of restaurant food to at-home food, if it persists?

# Need to do:
# 1. figure out which sectors to assign FAH and FAFH to
# 2. determine the monetary value of an average serving of FAH and FAFH
# 3. calculate environmental impacts per dollar and per serving for each of them

library(tidyverse)
library(lubridate)

# 1. which BEA sectors to assign FAH and FAFH to.

# FAFH: full-service restaurants, limited-service restaurants
# Blue Apron is in "other direct selling establishments" NAICS code 454390, but this may be poorly accounted for in the EEIO model.
# FAH: will assign to final demand categories of food, based on proportion of 2012 US consumption.

bea_full_svc <- '722110'
bea_limited_svc <- '722211'
bea_meal_kit <- '4A0000' # 454390: not well enough represented in model. Either ignore or add to the model.

# 2. determine monetary value of average serving of FAH and FAFH

# Extremely crude estimate of cost per serving of restaurant vs. meal kit vs. at home
# https://www.forbes.com/sites/priceonomics/2018/07/10/heres-how-much-money-do-you-save-by-cooking-at-home/ (2018)
# The average cost of restaurant is listed as $20.37, including a $5 delivery fee, based on a number of "fast casual" restaurants
# The average cost of meal kit is listed as $12.53, including a $2.50 delivery fee, based on popular meal kit companies
# The average cost of a home cooked meal is based on Whole Foods
cost_per_meal <- c(restaurant = 20.37, meal_kit = 12.53, home_cooked = 4.31)

# Also used Business Insider report on restaurant costs in 2015
# https://www.businessinsider.com/cost-to-eat-at-every-major-fast-food-chain-2015-9
restaurant_prices <- read_csv('/nfs/qread-data/scenario_inputdata/restaurant_prices.csv')
# Use the median cost at pizza, fast food, and fast casual for "limited service"
# Use the median cost at casual dining sit-down restaurants for "full service"

limited_svc <- restaurant_prices %>% filter(category %in% c('pizza', 'fast food', 'fast casual')) %>% summarize(cost = median(average_cost_per_person)) # $6.52.
full_svc <- restaurant_prices %>% filter(category %in% c('casual dining')) %>% summarize(cost = median(average_cost_per_person)) # $20.00.

# Got the food and beverage average CPI for the US from this source
# https://alfred.stlouisfed.org/series?seid=CPIFABSL
food_cpi <- read_csv('/nfs/qread-data/scenario_inputdata/food_cpi_bymonth.csv') 

food_cpi_annual <- food_cpi %>%
  mutate(DATE = as.Date(DATE, format = '%m/%d/%Y'),
         year = year(DATE)) %>%
  group_by(year) %>%
  summarize(CPI = mean(CPIFABSL_20200410))

# Get average cost of a meal at home and at various types of restaurant in 2012 dollars.
cost_per_meal <- c(limited_svc = limited_svc * with(food_cpi_annual, CPI[year == 2012]/CPI[year == 2015]),
                   full_svc = full_svc * with(food_cpi_annual, CPI[year == 2012]/CPI[year == 2015]),
                   meal_kit = (12.53 - 2.50) * with(food_cpi_annual, CPI[year == 2012]/CPI[year == 2018]),
                   home_cooked = 4.31 * with(food_cpi_annual, CPI[year == 2012]/CPI[year == 2018]))

