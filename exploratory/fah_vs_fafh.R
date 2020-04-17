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
library(reticulate)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub', '~'))


# Sectors to assign demand ------------------------------------------------



# FAFH: full-service restaurants, limited-service restaurants
# Blue Apron is in "other direct selling establishments" NAICS code 454390, but this may be poorly accounted for in the EEIO model.
# FAH: will assign to final demand categories of food, based on proportion of 2012 US consumption.

bea_full_svc <- '722110'
bea_limited_svc <- '722211'
bea_meal_kit <- '4A0000' # 454390: not well enough represented in model. Either ignore or add to the model.


# Cost per serving of FAH and FAFH ----------------------------------------



# Extremely crude estimate of cost per serving of restaurant vs. meal kit vs. at home
# https://www.forbes.com/sites/priceonomics/2018/07/10/heres-how-much-money-do-you-save-by-cooking-at-home/ (2018)
# The average cost of restaurant is listed as $20.37, including a $5 delivery fee, based on a number of "fast casual" restaurants
# The average cost of meal kit is listed as $12.53, including a $2.50 delivery fee, based on popular meal kit companies
# The average cost of a home cooked meal is based on Whole Foods
cost_per_meal <- c(restaurant = 20.37, meal_kit = 12.53, home_cooked = 4.31)

# Also used Business Insider report on restaurant costs in 2015
# https://www.businessinsider.com/cost-to-eat-at-every-major-fast-food-chain-2015-9
restaurant_prices <- read_csv(file.path(fp, 'scenario_inputdata/restaurant_prices.csv'))
# Use the median cost at pizza, fast food, and fast casual for "limited service"
# Use the median cost at casual dining sit-down restaurants for "full service"

limited_svc <- restaurant_prices %>% filter(category %in% c('pizza', 'fast food', 'fast casual')) %>% summarize(cost = median(average_cost_per_person)) # $6.52.
full_svc <- restaurant_prices %>% filter(category %in% c('casual dining')) %>% summarize(cost = median(average_cost_per_person)) # $20.00.

# Got the food and beverage average CPI for the US from this source
# https://alfred.stlouisfed.org/series?seid=CPIFABSL
food_cpi <- read_csv(file.path(fp, 'scenario_inputdata/food_cpi_bymonth.csv')) 

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


# Run EEIO for FAH and FAFH -----------------------------------------------



# Load EEIO model
if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

# Load 2012 final demand
finaldemand2012 <- read_csv(file.path(fp_github, 'USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'))

# Load the BEA code data (erroneously called NAICS) to get the codes
bea_codes <- read_csv(file.path(fp, 'crossreference_tables/naics_crosswalk_final.csv'))

# Demand codes table to convert 6 digit codes to the ones used by USEEIO
all_codes <- read_csv(file.path(fp, 'crossreference_tables/all_codes.csv'))

# Run EEIO for $1 and 1 serving for limited service restaurants and full service restaurants
eeio_full_svc_dollar <- eeio_lcia('USEEIO2012', as.list(1), as.list(with(all_codes, sector_desc_drc[sector_code_uppercase == bea_full_svc])))
eeio_full_svc_serving <- eeio_full_svc_dollar * cost_per_meal$full_svc
eeio_limited_svc_dollar <- eeio_lcia('USEEIO2012', as.list(1), as.list(with(all_codes, sector_desc_drc[sector_code_uppercase == bea_limited_svc])))
eeio_limited_svc_serving <- eeio_limited_svc_dollar * cost_per_meal$limited_svc

# Attempt to run for meal kit, although the values will likely be very inaccurate
eeio_meal_kit_dollar <- eeio_lcia('USEEIO2012', as.list(1), as.list(with(all_codes, sector_desc_drc[sector_code_uppercase == bea_meal_kit])))
eeio_meal_kit_serving <- eeio_limited_svc_dollar * cost_per_meal$meal_kit

# EEIO for home consumption. Must divide the $1 among all home purchase food categories.
fah_codes <- bea_codes %>% 
  filter(substr(BEA_389_code,1,1) %in% c('1','3'), proportion_food > 0) 

# Find 2012 US consumption for the FAH codes, then normalize so they sum to 1, accounting for the not-all-food categories.
fah_demand <- finaldemand2012 %>% 
  left_join(all_codes, by = c('BEA_389_code' = 'sector_code_uppercase')) %>%
  right_join(fah_codes) %>%
  mutate(relative_demand = (`2012_US_Consumption` * proportion_food) / sum(`2012_US_Consumption` * proportion_food))
  
eeio_home_cooking_dollar <- with(fah_demand, eeio_lcia('USEEIO2012', as.list(relative_demand), as.list(sector_desc_drc)))
eeio_home_cooking_serving <- eeio_home_cooking_dollar * cost_per_meal$home_cooked


# Plot results ------------------------------------------------------------

# Combine results into a single data frame.
fah_fafh_impacts <- rbind(map2_dfr(list(eeio_full_svc_dollar, eeio_limited_svc_dollar, eeio_meal_kit_dollar, eeio_home_cooking_dollar),
                                   c('full-service\nrestaurants', 'fast-food\nrestaurants', 'meal kits', 'home cooking'),
                                   ~ data.frame(sector = .y,
                                                category = row.names(.x),
                                                unit = 'dollar',
                                                impact = .x$Total)),
                          map2_dfr(list(eeio_full_svc_serving, eeio_limited_svc_serving, eeio_meal_kit_serving, eeio_home_cooking_serving),
                                   c('full-service\nrestaurants', 'fast-food\nrestaurants', 'meal kits', 'home cooking'),
                                   ~ data.frame(sector = .y,
                                                category = row.names(.x),
                                                unit = 'serving',
                                                impact = .x$Total)))



theme_set(theme_bw() +
            theme(legend.position = 'none',
                  strip.background = element_blank()))

category_names <- c('energy~(MJ)', 'greenhouse~gas~(kg~CO[2]~eq.)', 'land~(m^2)', 'water~(m^3)')

p_cost <- ggplot(data.frame(sector = c('full-service\nrestaurants', 'fast-food\nrestaurants', 'meal kits', 'home cooking'),
                            cost = unlist(cost_per_meal)[c(2,1,3,4)]),
                 aes(x = sector, y = cost, fill = sector)) +
  geom_col() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_brewer(palette = 'Dark2') +
  ggtitle('cost per serving')

p_serving <- fah_fafh_impacts %>%
  filter(unit == 'serving', grepl('co2|land|watr|enrg', category)) %>%
  mutate(category_label = rep(category_names, 4)) %>%
  ggplot(aes(x = sector, y = impact, fill = sector)) +
    geom_col() +
    facet_wrap(~ category_label, scales = 'free_y', labeller = label_parsed) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle('impact per serving')
  
p_dollar <- fah_fafh_impacts %>%
  filter(unit == 'dollar', grepl('co2|land|watr|enrg', category)) %>%
  mutate(category_label = rep(category_names, 4)) %>%
  ggplot(aes(x = sector, y = impact, fill = sector)) +
    geom_col() +
    facet_wrap(~ category_label, scales = 'free_y', labeller = label_parsed) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle('impact per dollar')

pdf(file.path(fp, 'figures/fah_vs_fafh_impacts.pdf'), height = 7, width = 8)
  print(p_cost)
  print(p_dollar)
  print(p_serving)
dev.off()
