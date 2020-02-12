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
tourism_hospitality <- codes_subset$BEA_389_code[c(4,6,9, 12, 14, 15, 16, 17, 18)] # Leave out movies and performances. Include air and ships.
## edit: leave out rail transport.
institutions <- codes_subset$BEA_389_code[c(19,20,22:27)] # Leave out other educational services since it does not appear to have food flows.

# 3 codes represent restaurants, 9 represent tourism/hospitality industry, 8 represent institutions that could adopt "WTA"

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

library(directlabels)

susb_bea_food_sums %>%
  filter(use) %>%
  mutate(receipts_per_firm = 1 + `Total receipts`/`No. firms`) %>%
  ggplot(aes(x = `Size class`, y = receipts_per_firm, group = BEA_Title)) + 
  geom_line(aes(color = BEA_Title)) + geom_dl(aes(label = BEA_Title), method = 'last.qp') +
  scale_y_log10(name = 'Receipts per firm') + theme_minimal() +
  theme(legend.position = 'none')
  
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

# Use the broad LAFA weighted means.


# Read LAFA and pull out weighted baseline rate means ---------------------

source(file.path(fp_github, 'fwe/read_data/read_lafa.r'))

# find the overall values for each of the 7 lafa groups
# The total one is the last one each time. Check this.
lafa <- list(dairy, fat, fruit, grain, meat, sugar, veg)
map(lafa, ~ rev(unique(.$Category))[1])

# If we can pull out the LAFA to BEA crosswalk, we may be able to get a better number.
# Steps: 
# (1) get relative % of purchases by BEA food group by $ value for each food service industry group.
# (2) convert the relative % by $ of BEA to relative % by $ of QFAHPD using the BEA-QFAHPD crosswalk.
# (3) convert the $ values to relative weights using the QFAHPD value to weight conversion factors (price per weight).
# (4) convert the relative weights of QFAHPD food groups to relative weights of LAFA groups using the crosswalk table.

# Step 1 can be done by taking them from the I-O table.
# It was already done in the partialsectorproportions.R script from a few months ago.
# Check and make sure the BEA groups included there include at least all the ones we are dealing with now.
# If not, will have to redo.

# Use table subset:
food_U <- read.csv(file.path(fp, 'crossreference_tables/level13_to_level4678_inputs.csv'), row.names = 1, check.names = FALSE)
# Check set diff
setdiff(proportion_affected$BEA_Code, colnames(food_U)) # they're all there.

food_U <- food_U[, proportion_affected$BEA_Code]
food_U <- food_U[rowSums(food_U) > 0, ]

# Load crosswalk(s) to see whether we have at least one QFAHPD category for each of the rows of food_U
lafa_qfahpd_bea_cwalk <- read_csv(file.path(fp, 'crossreference_tables/lafa_qfahpd_naics_crosswalk.csv'))

foodcodes <- row.names(food_U) # 35 different foods
setdiff(foodcodes, with(lafa_qfahpd_bea_cwalk, union(NAICS_raw, NAICS_processed))) # 13 are not. We need to make a new mapping maybe?

# Probably most beneficial to make a mapping starting with the food codes as rows and fill in 1 or more LAFA and QFAHPD. Comma separate to turn into list columns.

# bea_codes %>%
#   filter(BEA_389_code %in% foodcodes) %>%
#   select(BEA_389_code, BEA_389_def) %>%
#   write.csv(file = file.path(fp, 'crossreference_tables/bea_names_to_make_cw.csv'), row.names = FALSE)

# Beverage codes should be removed.
beveragecodes <- c('311920','311930','312110','312120','312130','312140')
food_U <- food_U[!row.names(food_U) %in% beveragecodes, ]

# Get the lafa names in a good order
# lafa_names <- list('fruit', 'veg', 'grain', 'dairy', 'meat', 'fat', 'sugar') %>% map_dfr(~ data.frame(group = ., food = unique(get(.)$Category)))
# write_csv(lafa_names, file.path(fp, 'crossreference_tables/lafa_names_ordered.csv'))

# Mapping will need to go bea --> qfahpd --> lafa
# Load the newly created crosswalks.
bea2qfahpd <- read_csv(file.path(fp, 'crossreference_tables/bea_qfahpd_crosswalk.csv'))
qfahpd2lafa <- read_csv(file.path(fp, 'crossreference_tables/qfahpd_lafa_crosswalk.csv'))

# Also load the QFAHPD data so that we can get the prices.
qfahpd2 <- read_csv(file.path(fp, 'raw_data/USDA/QFAHPD/tidy_data/qfahpd2.csv'))

# Convert the comma-separated string columns to list columns.
bea2qfahpd <- bea2qfahpd %>%
  mutate(QFAHPD_code = strsplit(QFAHPD_code, ';'))
qfahpd2lafa <- qfahpd2lafa %>%
  mutate(LAFA_names = strsplit(LAFA_names, ';'))

# Do the mapping of food_U to QFAHPD codes.
# Can be done in a "tidy" workflow
# Make it very long

food_U_QFAHPD <- food_U %>%
  mutate(BEA_389_code = row.names(food_U)) %>%
  pivot_longer(-BEA_389_code, names_to = 'BEA_recipient_code', values_to = 'monetary_flow') %>%
  left_join(bea2qfahpd %>% select(-BEA_389_def)) %>%
  group_by(BEA_389_code, BEA_recipient_code) %>%
  group_modify(~ data.frame(QFAHPD_code = .$QFAHPD_code[[1]], monetary_flow = .$monetary_flow/length(.$QFAHPD_code[[1]])))

# Now we have the use table where each BEA code has multiple rows for the different QFAHPD codes that make it up
# Create an aggregated version of QFAHPD to get the final price values for each code
# Weighted average across all market groups, years, and quarters
qfahpd_agg <- qfahpd2 %>%
  group_by(foodgroup) %>%
  summarize(price = weighted.mean(price, aggweight, na.rm = TRUE))

qfahpd_agg %>% print(n=nrow(.))

# Join the aggregated QFAHPD back up with its numerical codes and LAFA names

setdiff(qfahpd2lafa$QFAHPD_name, qfahpd_agg$foodgroup) # A couple of the dairy names are not correct. Can easily correct.


qfahpd_agg <- qfahpd_agg %>% 
  mutate(foodgroup = gsub('Whole and 2%', 'Regular fat', foodgroup)) %>%
  left_join(qfahpd2lafa, by = c('foodgroup' = 'QFAHPD_name')) %>%
  mutate(QFAHPD_code = as.character(QFAHPD_code))
# All match other than beverages.

# Now join the aggregated QFAHPD with the food_U mapped to QFAHPD so that the total $ can be divided by $/weight to yield a weight (or mass).
# Units do not really matter
food_U_LAFA <- food_U_QFAHPD %>%
  left_join(qfahpd_agg) %>%
  mutate(mass_flow = monetary_flow / price)

# For the unique LAFA names in the QFAHPD to LAFA mapping, extract the waste rates for 2012 or the closest year post-2012.
lafa_to_extract <- Reduce(union, qfahpd2lafa$LAFA_names)


# Split it up again by LAFA so that we can get the weights.
# Get only the columns we care about from each LAFA element in the list
# Then get the year closest to 2012
lafa_df <- lafa %>% 
  map_dfr(~ select(., Category, Year, Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent, Consumer_weight_Lbs.year)) %>%
  rename(avoidable_consumer_loss = Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent,
         consumer_weight = Consumer_weight_Lbs.year) %>%
  filter(!is.na(avoidable_consumer_loss)) %>%
  group_by(Category) %>%
  summarize(avoidable_consumer_loss = avoidable_consumer_loss[which.min(abs(Year-2012))],
            consumer_weight = consumer_weight[which.min(abs(Year-2012))])

setdiff(lafa_to_extract, lafa_df$Category)
# The following categories will need to be used to get avoidable consumer loss percentage
# Calculate weighted average of avoidable pct loss, weighted by weight available to consumers in the given year.
# [1] "Fresh fruit"                 "Frozen fruit"                "Canned fruit"                "Juice"                      
# [5] "Canned vegetables"           "Fresh vegetables"            "Frozen vegetables"           "Dry edible beans"           
# [9] "Total grains"                "Total cheese"                "Red meat"                    "Poultry"                    
# [13] "Total Fresh and Frozen Fish" "Canned fish and shellfish"   "Total tree nuts"             "Caloric sweeteners"         
# [17] "beverage"                    "prepared food"              

# Read the description of the nested category structure in.
lafa_struct <- read_csv(file.path(fp, 'crossreference_tables/lafa_category_structure.csv'))

lafa_df <- lafa_df %>%
  left_join(lafa_struct, by = c('Category' = 'Food'))

lafa_group_rates <- map_dfr(1:4, function(i) lafa_df %>% 
  rename_(subgroup = paste0('subgroup', i)) %>%
  group_by(subgroup) %>% 
  summarize(avoidable_consumer_loss = weighted.mean(x = avoidable_consumer_loss, w = consumer_weight))) %>%
  filter(!is.na(subgroup))

# Overall mean for other category
overall_mean <- with(lafa_df, weighted.mean(avoidable_consumer_loss, consumer_weight))

all_lafa_rates <- data.frame(Category = c(lafa_df$Category, lafa_group_rates$subgroup, 'prepared food'),
                             avoidable_consumer_loss = c(lafa_df$avoidable_consumer_loss, lafa_group_rates$avoidable_consumer_loss, overall_mean))

# Rates of consumer-level "avoidable" waste by food group in LAFA (262 different foods)
ggplot(all_lafa_rates, aes(x = avoidable_consumer_loss/100)) + 
  geom_histogram() +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), name = 'Avoidable consumer food waste') +
  scale_y_continuous(expand = c(0,0.1))


# Map mass flows to LAFA groups -------------------------------------------

# Use the same pipe as last time to spread out the LAFA names over the rows

food_U_LAFA_spread <- food_U_LAFA %>%
  group_by(BEA_389_code, BEA_recipient_code, QFAHPD_code, price) %>%
  group_modify(~ data.frame(LAFA_name = .$LAFA_names[[1]], 
                            mass_flow = .$mass_flow/length(.$LAFA_names[[1]]),
                            monetary_flow = .$monetary_flow/length(.$LAFA_names[[1]]))) %>%
  left_join(all_lafa_rates, by = c('LAFA_name' = 'Category'))
 
# Join with the proportion of sales affected (also the same as proportion of mass affected) based on % sales that are food and the >20 employee threshold
# Then calculate the reduction in required mass flow for each food type, and then a weighted average to get the reduction in monetary flow needed
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

food_U_LAFA_spread <- food_U_LAFA_spread %>% 
  left_join(proportion_affected, by = c('BEA_recipient_code' = 'BEA_Code')) %>%
  mutate(reduction_by_mass = demand_change_fn(W0 = avoidable_consumer_loss/100, r = overall_waste_reduction, p = proportion_receipts_affected),
         mass_flow_post_intervention = mass_flow * reduction_by_mass,
         monetary_flow_post_intervention = mass_flow_post_intervention * price)

# Sum up by old row and column index from the original food_U matrix, then reshape to make the same matrix.
food_U_postintervention_df <- food_U_LAFA_spread %>%
  group_by(BEA_389_code, BEA_recipient_code) %>%
  summarize(monetary_flow = sum(monetary_flow_post_intervention, na.rm = TRUE)) 

food_U_postintervention <- food_U_postintervention_df %>%
  pivot_wider(names_from = BEA_recipient_code, values_from = monetary_flow, values_fill = list(monetary_flow = 0)) %>%
  as.data.frame
row.names(food_U_postintervention) <- food_U_postintervention$BEA_389_code
food_U_postintervention <- food_U_postintervention[, !names(food_U_postintervention) %in% 'BEA_389_code']

# Make the row and column order the same as food_U
row.names(food_U_postintervention) == row.names(food_U) # Good
food_U_postintervention <- food_U_postintervention[, names(food_U)]

# Summarize mass waste reduction and $ reduction --------------------------

# The % of waste reduced by mass won't be exactly equivalent to the % of waste reduced by $.

postintervention <- food_U_postintervention_df %>%
  group_by(BEA_recipient_code) %>%
  summarize(monetary_flow_postintervention = sum(monetary_flow))

preintervention <- data.frame(BEA_recipient_code = names(food_U), monetary_flow_preintervention = colSums(food_U))

monetary_byintervention <- left_join(postintervention, preintervention) %>%
  mutate(reduction_bydollar = 1 - monetary_flow_postintervention / monetary_flow_preintervention) 

mass_byintervention <- food_U_LAFA_spread %>%
  group_by(BEA_recipient_code) %>%
  summarize(mass_flow_preintervention = sum(mass_flow, na.rm = TRUE),
            mass_flow_postintervention = sum(mass_flow_post_intervention, na.rm = TRUE)) %>%
  mutate(reduction_bymass = 1 - mass_flow_postintervention / mass_flow_preintervention) 

rate_changes <- left_join(mass_byintervention, monetary_byintervention)

# Proportion reduction in monetary units is systematically higher than in mass units but 
ggplot(rate_changes) +
  geom_point(aes(x = reduction_bymass, y = reduction_bydollar)) +
  geom_abline(slope=1, linetype = 'dotted') +
  annotate(geom = 'text', x = 0.095, y = 0.094, label = "1:1 line", angle = 45) +
  theme_minimal() +
  labs(x = 'proportion reduction by mass', y = 'proportion reduction by $') +
  theme(aspect.ratio = 1)

# A better way is to phrase it as reducing the operating costs (purchases of raw materials) by the affected industries
# Since we have a waste rate reduction by mass, convert it back to waste rate reduction by money and get the purchasing rate reduction from each of the industries that supply the final foodservice industries.

# Calculate pre and post incoming monetary food flow in millions of dollars
total_prepost <- data.frame(BEA_Code = names(food_U),
                            food_purchases_baseline = colSums(food_U),
                            food_purchases_postintervention = colSums(food_U_postintervention)) %>%
  mutate(reduction = food_purchases_baseline - food_purchases_postintervention)



# Weighted mean of waste rate by mass flow for each foodservice industry = final waste rate for the industries!
baseline_waste_foodservice <- food_U_LAFA_spread %>%
  group_by(BEA_recipient_code) %>%
  summarize(avoidable_consumer_loss = weighted.mean(x = avoidable_consumer_loss, w = mass_flow, na.rm = TRUE))

# One is missing (other education) so we will apply the rate for colleges and universities to it
# baseline_waste_foodservice$avoidable_consumer_loss[baseline_waste_foodservice$BEA_recipient_code == '611B00'] <- baseline_waste_foodservice$avoidable_consumer_loss[baseline_waste_foodservice$BEA_recipient_code == '611A00']
# No longer needed since other education has no food purchases.

# Join up the names of the BEA codes and print the table of values
trunc_ellipsis <- function(x, n) if_else(nchar(x) < 30, x, paste(substr(x, 1, n), '...'))
baseline_waste_foodservice <- baseline_waste_foodservice %>% 
  left_join(codes_subset, by = c('BEA_recipient_code' = 'BEA_389_code')) %>%
  setNames(c('BEA_Code', 'baseline', 'BEA_Title')) %>%
  left_join(bea_to_use_df)

baseline_waste_foodservice <- baseline_waste_foodservice %>%
  left_join(proportion_affected %>% select(BEA_Code, proportion_receipts_affected, proportion_food)) %>%
  left_join(total_prepost)

baseline_waste_foodservice %>%
  mutate(BEA_Title = trunc_ellipsis(BEA_Title, 30)) %>%
  select(sector, BEA_Title, baseline, food_purchases_baseline, food_purchases_postintervention, reduction) %>%
  rename(baseline_waste_percent = baseline) %>%
  arrange(sector) %>%
  print(n = nrow(.))


# The demand on which to run the eeio -------------------------------------

# Simply enough, just run it on the difference in the two rowSums for pre and post intervention
# This represents final operating costs of the industries, as if it were final consumer demand

# Above they were marginal column totals for the recipient industries
# Phrase it also as marginal row totals for the food types

reduction_byfoodtype <- data.frame(BEA_Code = row.names(food_U),
                                   cost_averted = rowSums(food_U) - rowSums(food_U_postintervention))

# For display purposes join this with the name of the food so that we can see the names of the rows being totaled up.
bea_codes %>%
  select(BEA_389_code, BEA_389_def) %>%
  rename(BEA_Code = BEA_389_code, BEA_Title = BEA_389_def) %>%
  right_join(reduction_byfoodtype) %>%
  mutate(BEA_Title = trunc_ellipsis(BEA_Title, 30)) %>%
  print(n = nrow(.))

### RUN EEIO on this demand vector.

library(reticulate)

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

# The model is already built so we don't need to build it again. 
# All we need to do is match the demand vector 6 digit codes with the codes that include the full names
# then run eeio_lcia on it.

# Demand codes table
all_codes <- read_csv(file.path(fp, 'crossreference_tables/all_codes.csv'))

demand_vector <- reduction_byfoodtype %>%
  left_join(all_codes, by = c('BEA_Code' = 'sector_code_uppercase')) %>%
  with(list(codes = sector_desc_drc, values = cost_averted))

eeio_wta <- eeio_lcia('USEEIO2012', demand_vector$values * 1e6, demand_vector$codes) 

# Also do for the baseline
baseline_byfoodtype <- data.frame(BEA_Code = row.names(food_U),
                                   baseline = rowSums(food_U))

demand_vector_baseline <- baseline_byfoodtype %>%
  left_join(all_codes, by = c('BEA_Code' = 'sector_code_uppercase')) %>%
  with(list(codes = sector_desc_drc, values = baseline))

eeio_wta_baseline <- eeio_lcia('USEEIO2012', demand_vector_baseline$values * 1e6, demand_vector_baseline$codes) 


# Make a plot of the result -----------------------------------------------

# A table is probably the best way to show it
eeio_dat <- data.frame(category = row.names(eeio_wta),
                       baseline = eeio_wta_baseline$Total,
                       impact_averted = eeio_wta$Total)

eeio_dat %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate(baseline = baseline * c(1e-9, 1e-6, 1e-9, 1e-10, 1e-9),
         impact_averted = impact_averted * c(1e-9, 1e-6, 1e-9, 1e-10, 1e-9),
         category = c('energy (PJ)', 'eutrophication (kT N)', 'greenhouse gas (MT CO2)', 'land (Mha)', 'water (km3)'),
         percent_averted = signif(100 * impact_averted/baseline, 2))


# Total cost and divide by impact -----------------------------------------

# Cost is total n of establishments * cost per establishment

# Print table
establishments_affected %>% 
  ungroup %>%
  filter(use) %>%
  select(-use) %>%
  mutate(BEA_Title = trunc_ellipsis(BEA_Title, 30))

n_estab <- sum(establishments_affected$establishments[establishments_affected$use])
total_cost <- c(8749, 20102) * n_estab # 4 to 9 billion

cost_per_impact <- eeio_dat %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate(cost_per_reduction_lower = total_cost[1] / impact_averted,
         cost_per_reduction_upper = total_cost[2] / impact_averted)

cost_per_impact %>%
  select(-impact_averted, -baseline) %>%
  mutate(category = c('energy ($/MJ)', 'eutrophication ($/kg N)', 'greenhouse gas ($/kg CO2)', 'land ($/m2)', 'water ($/m3)'),
         cost_per_reduction_lower = paste0('$', round(cost_per_reduction_lower, 2)),
         cost_per_reduction_upper = paste0('$', round(cost_per_reduction_upper, 2)))

         
         
         