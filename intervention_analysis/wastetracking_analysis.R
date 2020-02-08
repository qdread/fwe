# Impact analysis of waste tracking and analytics adoption (e.g. Leanpath)
# QDR / FWE / 04 Feb 2020

# Using assumptions and sources documented in costs_waste_tracking_analytics.xlsx and platewaste.xlsx

# Procedure:
# Find number of establishments and size of establishments that can implement WTA
# Find cost (one-time + amortized annual cost) of implementation
# Find savings and benefits (use EEIO to get environmental benefits)


# Load data ---------------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub', '~'))

#source(file.path(fp_github, 'fwe/USEEIO/load_scenario_data.r'))
#source(file.path(fp_github, 'fwe/figs/theme_black.R'))

# Load numbers of establishments by NAICS
susb_naics <- read_csv(file.path(fp, 'csv_exports/SUSB_NAICS_allvariables.csv'))
# Load the BEA code data (erroneously called NAICS)
bea_codes <- read_csv(file.path(fp, 'crossreference_tables/naics_crosswalk_final.csv'))
# Load NAICS BEA crosswalks
load(file.path(fp, 'crossreference_tables/NAICS_BEA_SCTG_crosswalk.RData'))
bea_naics <- read_csv(file.path(fp, 'crossreference_tables/BEA_NAICS07_NAICS12_crosswalk.csv'))

# For now, use the final output values from the cost sheet Mary made.
cost_range <- c(lower = 8749, upper = 20102) # Lower and upper bounds for annual cost, based on percentiles for employee hourly wages.

waste_reduction <- c(lower = 0.4, upper = 0.5, mode = 0.45) # Waste reduction achievable, rates from ReFED?

prop_kitchen_waste <- c(lower = 0.7, upper = 0.97, mode = 0.85) # Proportion kitchen waste (all other than plate waste), estimated from a few sources

# Number and size of establishments ---------------------------------------

# Identify BEA (and/or NAICS) codes that are targeted for intervention.

bea_codes %>% filter(stage %in% c('foodservice', 'institutional')) %>% select(BEA_389_code, BEA_389_def) %>% print(n=nrow(.))

codes_subset <- bea_codes %>% filter(stage %in% c('foodservice', 'institutional')) %>% select(BEA_389_code, BEA_389_def)

# Main categories:
# - Restaurants
# - Hospitality industry
# - Tourism/recreation industry
# - Institutions and community services

# Restaurants to include full-service, limited-service, and "other"
# Do not include transportation
# Note: may include some types of transportation e.g. cruises, passenger air and rail, that could use food WTA.

# For other consumer foodservice not within the 3 restaurant categories, include (by proportion)
# Movies, performance, sports, sightseeing transportation and support activities, museums, amusement parks, gambling establishments, golf course/rec, 
# hotels

# For institutional foodservice, include (by proportion)
#  Schools/colleges, hospitals, nursing facilities, residential facilities, day care, community service, (do not include religious organizations, civic organizations),
#  other government institutions.

restaurants <- codes_subset$BEA_389_code[1:3]
tourism_hospitality <- codes_subset$BEA_389_code[c(9, 10, 11, 12, 14, 15, 16, 17, 18)]
institutions <- codes_subset$BEA_389_code[c(19:27, 31:34)]

# Look at transportation codes.
bea_naics %>% filter(substr(BEA_Code,1,2) == '48') %>% print(n=nrow(.))

# Get the relevant part of the bea/naics crosswalk
bea_to_use <- c(restaurants, tourism_hospitality, institutions)

bea_naics_to_use <- bea_naics %>% filter(BEA_Code %in% bea_to_use)

# See whether there are any discrepancies between the codes.
length(unique(bea_naics_to_use$BEA_Code)) # Only 21 of them are in there, damn.
bea_to_use[!bea_to_use %in% bea_naics$BEA_Code] # It is the government ones. We will have to leave them out.

bea_naics_to_use[which(bea_naics_to_use$related_2007_NAICS_6digit != bea_naics_to_use$related_2012_NAICS_6digit), ]
# Restaurant codes have changed.

# Which are used?
susb_naics %>% filter(substr(NAICS, 1, 3) == '722') %>% pull(NAICS) %>% unique # 2012 codes. 

# Get all NAICS codes to be used
naics_to_use <- unique(bea_naics_to_use$related_2012_NAICS_6digit)

setdiff(naics_to_use, susb_naics$NAICS) 


# Final version of NAICS to use -------------------------------------------

restaurants <- codes_subset$BEA_389_code[1:3]
tourism_hospitality <- codes_subset$BEA_389_code[c(4,5,6,9, 12, 14, 15, 16, 17, 18)] # Leave out movies and performances. Include air, rail, and ships.
institutions <- codes_subset$BEA_389_code[c(19:27)]

# 3 codes represent restaurants, 10 represent tourism/hospitality industry, 9 represent institutions that could adopt "WTA"

bea_to_use_df <- data.frame(BEA_Code = c(restaurants, tourism_hospitality, institutions),
                            sector = rep(c('restaurants', 'tourism and hospitality', 'institutions'), c(length(restaurants), length(tourism_hospitality), length(institutions))))

bea_naics_to_use <- bea_to_use_df %>% left_join(bea_naics)
any(duplicated(bea_naics_to_use$related_2012_NAICS_6digit)) # There are no duplicates. 96 unique NAICS codes.

# Create subset with only food service.
susb_naics_foodservice <- bea_naics_to_use %>%
  select(BEA_Code, BEA_Title, sector, related_2012_NAICS_6digit) %>%
  rename(NAICS = related_2012_NAICS_6digit) %>%
  left_join(susb_naics)

# Flag rows that aren't going to be used (for example, freight transportation industries within transportation codes)
# Also remove campgrounds within the hospitality industry.
unique(susb_naics_foodservice$`NAICS description`)
words_to_remove <- c('Freight', 'Nonscheduled', 'Air Traffic', 'Support Activities', 'Port', 'Cargo', 'Navigational', 'Towing', 'Packing', 'Campground')

susb_naics_foodservice <- susb_naics_foodservice %>%
  mutate(use = !grepl(paste(words_to_remove, collapse = '|'), `NAICS description`),
         `Size class` = factor(`Size class`, levels = c('fewer than 20', '20 to 99', '100 to 499', 'more than 500', 'total')))

# Find threshold of institution size to adopt WTA so that the volume of sales associated with establishments that adopt WTA 
# lines up with ReFED's assumptions that 80% of institutions and 15% of restaurants will adopt.

susb_food_sums <- susb_naics_foodservice %>% 
  group_by(sector, use, `Size class`) %>%
  summarize_at(vars(`No. firms`:`Total receipts`), sum) %>%
  filter(!is.na(`Size class`), !`Size class` %in% 'total')

# What are the proportions

susb_food_cumul_prop <- susb_food_sums %>%
  mutate_at(vars(`No. firms`:`Total receipts`), ~ cumsum(.x) / sum(.x))

# For institutions, only 4% of payroll is represented by fewer than 20 employees. For restaurants, 24% by fewer than 20. For tourism industry, in the part we are considering, 12% of payroll is represented by fewer than 20 employees.

# Check out this power law graph.
susb_food_cumul_prop %>% 
  filter(use) %>%
  ggplot(aes(x = `No. firms`, y = `Total receipts`, color = sector)) + geom_line() + scale_x_log10(name = 'Proportion of establishments by increasing size') + scale_y_log10('Proportion of sales')

susb_food_sums %>%
  mutate(employees_per_firm = `No. employees`/`No. firms` ) %>%
  filter(use) %>%
  ggplot(aes(x = employees_per_firm, y = `No. firms`, color = sector)) + geom_line() + scale_x_log10(name = 'Number of employees per firm') + scale_y_log10(name = 'Number of firms in size class', breaks = c(1000, 10000, 100000), limits = c(1000, 400000)) + theme_minimal()

susb_food_sums %>%
  mutate(employees_per_firm = `No. employees`/`No. firms` ) %>%
  filter(use) %>%
  ggplot(aes(x = employees_per_firm, y = `Total receipts`, color = sector)) + geom_line() + scale_x_log10(name = 'Number of employees per firm') + scale_y_log10(name = 'Total receipts in size class') + theme_minimal()

# We can use 20 as the threshold. It can be altered.

size_classes_exclude <- expand_grid(sector = c('restaurants', 'tourism and hospitality', 'institutions'), `Size class` = levels(susb_food_sums$`Size class`)[1:4]) %>%
  mutate(exclude = `Size class` %in% c('fewer than 20'))
                                    
                               

# Next, find the "proportion food" for each of the final groups so we can determine what % of the industry demand to modify.
# This should account for either the % of food inputs at the BEA group level, or the % of employees in the NAICS groups within it, or both.


# Get proportions by bea and naics ----------------------------------------

# Within each BEA code, get the percentage of total receipts that will be affected by WTA implementation
# All establishments with >20 employees, and also account for the fact that some tourism and hospitality BEA codes contain NAICS codes that aren't going to adopt WTA

susb_bea_food_sums <- susb_naics_foodservice %>%
  group_by(sector, BEA_Code, BEA_Title, use, `Size class`) %>%
  summarize_at(vars(`No. firms`:`Total receipts`), sum) %>%
  filter(!is.na(`Size class`), !`Size class` %in% 'total')

# Inspect this to see if there are any weird things
# The receipts per firm should increase with size class, but some are zeroes or low
# This may be due to data being censored (if some size classes have very few firms the data might be censored for privacy reasons)

susb_bea_food_sums %>%
  filter(use) %>%
  mutate(receipts_per_firm = `Total receipts`/`No. firms`) %>%
  ggplot(aes(x = `Size class`, y = receipts_per_firm, group = interaction(BEA_Title, use), color = interaction(BEA_Title, use))) + 
    geom_line() + scale_y_log10()
  
# According to this graph and to inspecting the data, air transportation and water transportation have $0 receipts for firms greater than 500 employees
# Obviously this is wrong.
# Use the relationship between payroll and receipts to impute receipts for the large firms in those 2 sectors.

recbyestb <- susb_bea_food_sums %>%
  filter(use) %>%
  mutate(empl_per_firm = `No. employees`/`No. firms`,
         receipts_per_estb = `Total receipts`/`No. establishments`,
         receipts_per_firm = `Total receipts`/`No. firms`) 

recbyestb %>% 
  filter(BEA_Code %in% c('481000', '483000'), receipts_per_estb > 0) %>%
  ggplot(aes(x = empl_per_firm, y = receipts_per_estb, group = BEA_Title, color = BEA_Title)) + geom_line() + scale_x_log10() + scale_y_log10()

recbyestb %>% 
  filter(BEA_Code %in% c('481000', '483000'), receipts_per_firm > 0) %>%
  ggplot(aes(x = empl_per_firm, y = receipts_per_firm, group = BEA_Title, color = BEA_Title)) + geom_line() + scale_x_log10() + scale_y_log10()

# The log ratio of receipts per firm increases linearly with the log ratio of employees per firm. 
# For air transit, we know the average number of employees per firm is 10,087

# Impute the air transit number.
lm_air <- lm(log(receipts_per_firm) ~ log(empl_per_firm), data = recbyestb, subset = BEA_Title == 'Air transportation' & receipts_per_firm > 0)

predicted_air <- exp(predict(lm_air, newdata = recbyestb %>% filter(BEA_Title == 'Air transportation') %>% select(empl_per_firm)))
predicted_air[4] # The imputed value for air transportation total receipts for firms with 500 or more employees.

# For water transportation, our problem is that we don't know either the average number of employees per firm or the receipts, the employees were
# also clearly censored wince only 202 employees for 11 firms is a lot less than 500 per firm. 
# Let's look for a similar industry to see whether we can get a number of employees per firm to use to impute.
lm_water <- lm(log(receipts_per_firm) ~ log(empl_per_firm), data = recbyestb, subset = BEA_Title == 'Water transportation' & receipts_per_firm > 0)

susb_bea_food_sums %>%
 mutate(empl_per_firm = `No. employees`/`No. firms`) %>%
 filter(`Size class` == 'more than 500')

# We will use the number from scenic transportation (700) which seems fairly conservative
# This number can be sampled from in an uncertainty analysis too.
recbyestb %>% filter(BEA_Title == 'Water transportation') %>% mutate(empl_per_firm = if_else(`Size class` == 'more than 500', 700, empl_per_firm)) %>% select(empl_per_firm)
predicted_water <- exp(predict(lm_water, newdata = recbyestb %>% filter(BEA_Title == 'Water transportation') %>% mutate(empl_per_firm = if_else(`Size class` == 'more than 500', 700, empl_per_firm)) %>% select(empl_per_firm)))
exp(predict(lm_water))

# Get the two imputed values
c(predicted_air[4], predicted_water[4])

# Create new imputed dataset.
susb_bea_food_sums[susb_bea_food_sums$BEA_Code %in% c('481000', '483000') & susb_bea_food_sums$use & susb_bea_food_sums$`Size class` %in% 'more than 500', "Total receipts"] <- c(predicted_air[4], predicted_water[4])

#### Here are the proportions of receipts that will be affected by adoption of WTA

susb_bea_proportion_affected <- susb_bea_food_sums %>% 
  ungroup %>%
  left_join(size_classes_exclude) %>%
  mutate(use = use & !exclude) %>%
  group_by(sector, BEA_Code, BEA_Title) %>%
  summarize(proportion_receipts_affected = sum(`Total receipts`[!exclude])/sum(`Total receipts`))

# Now multiply this by "proportion food" for each of the industries
prop_foods <- bea_codes %>%
  select(BEA_389_code, proportion_food) %>%
  rename(BEA_Code = BEA_389_code)

proportion_affected <- susb_bea_proportion_affected %>% left_join(prop_foods) %>% mutate(final_proportion = proportion_receipts_affected * proportion_food)

# We also need number of establishments affected so we can get the total annual cost
establishments_affected <- susb_bea_food_sums %>% 
  ungroup %>%
  left_join(size_classes_exclude) %>%
  mutate(use = use & !exclude) %>%
  group_by(sector, BEA_Code, BEA_Title, use) %>%
  summarize(establishments = sum(`No. establishments`))

# Make a barplot of establishments affected

ggplot(establishments_affected %>% ungroup %>% mutate(BEA_Title = factor(BEA_Title, levels = unique(BEA_Title))), aes(y = establishments, x = BEA_Title, fill = sector, alpha = use)) + 
  geom_bar(position = 'stack', stat = 'identity') +
  scale_y_continuous(name = 'Number of establishments', limits = c(0, 3e5), expand = c(0,0)) +
  scale_fill_brewer(palette = 'Set2') +
  labs(alpha = 'Can adopt') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Number of establishments that can adopt waste tracking', 'based on having >20 employees and inclusion in a NAICS code that involves providing food')

# Make a barplot of the proportion of the environmental impact that is affected
ggplot(proportion_affected %>% ungroup %>% mutate(BEA_Title = factor(BEA_Title, levels = unique(BEA_Title))), aes(y = final_proportion, x = BEA_Title, fill = sector)) + 
  geom_bar(stat = 'identity') +
  scale_y_continuous(name = 'Proportion of sales affected', limits = c(0,1), expand = c(0,0)) +
  scale_fill_brewer(palette = 'Set2') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Proportion of sales by BEA industry that will be influenced by waste tracking', 'based on proportion of expenses in each sector due to food, and excluding firms with <20 employees')

# Next, modify this proportion again by the % demand reduction we will get due to food waste reduction.
# That is going to be the proportion kitchen waste reduction you can get from WTA * the proportion of food waste that is in the kitchen versus "plate waste" generated by the customers

# This is going to be the same for all. We will use the "mode"
(overall_waste_reduction <- prop_kitchen_waste["mode"] * waste_reduction["mode"])

# Assuming 45% kitchen waste reduction, and 85% of establishments' waste is generated in the kitchen.

# Therefore, if food waste is reduced by 38.25%, how much $ less of food does the industry have to purchase?
# That also depends upon the baseline rate of food waste.
# Can we use LAFA in this case? 
# Let's use consumer, unavoidable, weighted by food group.
# We can directly use the LAFA <--> BEA mapping -- holy shit I finally get to use that thing I spent so long making.

# We will use consumer waste % relative to the weight that makes it to the consumer level.

# Here we will not include beverages. Only include the lafa categories:
# dairy, fat, fruit, grain, meat, sugar, vegetables