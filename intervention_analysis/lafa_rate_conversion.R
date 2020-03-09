# LAFA waste rate calculations
# QDR / FWE / 09 Mar 2020

# Load data for waste rate calc -------------------------------------------

# Load packages and check whether this is being run locally or on rstudio server.
library(tidyverse)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub', '~'))

# Load the BEA code data (erroneously called NAICS)
bea_codes <- read_csv(file.path(fp, 'crossreference_tables/naics_crosswalk_final.csv'))
# Load NAICS BEA crosswalks
load(file.path(fp, 'crossreference_tables/NAICS_BEA_SCTG_crosswalk.RData'))
bea_naics <- read_csv(file.path(fp, 'crossreference_tables/BEA_NAICS07_NAICS12_crosswalk.csv'))
# Load LAFA
source(file.path(fp_github, 'fwe/read_data/read_lafa.r'))
lafa <- list(dairy, fat, fruit, grain, meat, sugar, veg)

# BEA levels 1+3 to 4+6+7+8 is already subsetted from an older analysis I did.
food_U <- read.csv(file.path(fp, 'crossreference_tables/level13_to_level4678_inputs.csv'), row.names = 1, check.names = FALSE)

# Mapping will need to go bea --> qfahpd --> lafa
# Load the two necessary crosswalks.
bea2qfahpd <- read_csv(file.path(fp, 'crossreference_tables/bea_qfahpd_crosswalk.csv'))
qfahpd2lafa <- read_csv(file.path(fp, 'crossreference_tables/qfahpd_lafa_crosswalk.csv'))

# Also load the QFAHPD data so that we can get the prices.
qfahpd2 <- read_csv(file.path(fp, 'raw_data/USDA/QFAHPD/tidy_data/qfahpd2.csv'))

# Read the description of LAFA's nested category structure in.
lafa_struct <- read_csv(file.path(fp, 'crossreference_tables/lafa_category_structure.csv'))

# Demand codes table to convert 6 digit codes to the ones used by USEEIO
all_codes <- read_csv(file.path(fp, 'crossreference_tables/all_codes.csv'))

# Beverage codes should be removed.
beveragecodes <- c('311920','311930','312110','312120','312130','312140')
food_U <- food_U[!row.names(food_U) %in% beveragecodes, ]

# BEA codes that do not appear in the QFAHPD or LAFA
unused_bea_codes <- setdiff(y=bea2qfahpd$BEA_389_code, x=row.names(food_U))
food_U <- food_U[!row.names(food_U) %in% unused_bea_codes, ]

# Get rid of unneeded rows and columns
food_U <- food_U[rowSums(food_U) > 0, ]
food_U <- food_U[, colSums(food_U) > 0]



# Map BEA to QFAHPD -------------------------------------------------------

# Convert the comma-separated string columns to list columns.
bea2qfahpd <- bea2qfahpd %>%
  mutate(QFAHPD_code = strsplit(QFAHPD_code, ';'))
qfahpd2lafa <- qfahpd2lafa %>%
  mutate(LAFA_names = strsplit(LAFA_names, ';'))

# Do the mapping of food_U to QFAHPD codes.
# This is done with even weightings.
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

# Join the aggregated QFAHPD back up with its numerical codes and LAFA names
# Meanwhile correct a couple wrong names in the dairy category
qfahpd_agg <- qfahpd_agg %>% 
  mutate(foodgroup = gsub('Whole and 2%', 'Regular fat', foodgroup)) %>%
  left_join(qfahpd2lafa, by = c('foodgroup' = 'QFAHPD_name')) %>%
  mutate(QFAHPD_code = as.character(QFAHPD_code))

# Now join the aggregated QFAHPD with the food_U mapped to QFAHPD so that the total $ can be divided by $/weight to yield a weight (or mass).
# The units mass is in don't matter since they are all relative
food_U_LAFA <- food_U_QFAHPD %>%
  left_join(qfahpd_agg) %>%
  mutate(mass_flow = monetary_flow / price)


# Get LAFA rates including lower-level averages ---------------------------

# For the unique LAFA names in the QFAHPD to LAFA mapping, extract the waste rates for 2012 or the closest year post-2012.
lafa_to_extract <- Reduce(union, qfahpd2lafa$LAFA_names)

# Split it up again by LAFA so that we can get the weights.
# Get only the columns we care about from each LAFA element in the list
# In this version, include all types of waste
# Then get the year closest to 2012
lafa_df <- lafa %>% 
  map_dfr(~ select(., Category, Year, 
                   Primary_weight_Lbs.year, Loss_from_primary_to_retail_weight_Percent, 
                   Retail_weight_Lbs.year, Loss_from_retail__institutional_to_consumer_level_Percent, 
                   Consumer_weight_Lbs.year, Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent)) %>%
  setNames(c('Food','Year','primary_weight', 'primary_loss', 'retail_weight','retail_loss','consumer_weight','avoidable_consumer_loss')) %>%
  filter(!is.na(avoidable_consumer_loss)) %>%
  group_by(Food) %>%
  filter(abs(Year - 2012) == min(abs(Year - 2012)))

# Use nested category structure to get weighted mean rates for the coarser LAFA groups
# for QFAHPD foods that do not resolve to the finest available level of LAFA
lafa_df <- lafa_df %>%
  left_join(lafa_struct)

lafa_group_rates <- map_dfr(1:4, function(i) lafa_df %>% 
                              rename_(subgroup = paste0('subgroup', i)) %>%
                              group_by(subgroup) %>% 
                              summarize(primary_loss = weighted.mean(x = primary_loss, w = primary_weight, na.rm = TRUE),
                                        retail_loss = weighted.mean(x = retail_loss, w = retail_weight, na.rm = TRUE),
                                        avoidable_consumer_loss = weighted.mean(x = avoidable_consumer_loss, w = consumer_weight, na.rm = TRUE),
                                        primary_weight = sum(primary_weight, na.rm = TRUE),
                                        retail_weight = sum(retail_weight, na.rm = TRUE),
                                        consumer_weight = sum(consumer_weight, na.rm = TRUE))) %>%
  filter(!is.na(subgroup))

# Use LAFA overall mean for prepared food in the "other" category
overall_mean <- lafa_df %>%
  ungroup %>%
  summarize(primary_loss = weighted.mean(x = primary_loss, w = primary_weight, na.rm = TRUE),
            retail_loss = weighted.mean(x = retail_loss, w = retail_weight, na.rm = TRUE),
            avoidable_consumer_loss = weighted.mean(x = avoidable_consumer_loss, w = consumer_weight, na.rm = TRUE),
            primary_weight = sum(primary_weight, na.rm = TRUE),
            retail_weight = sum(retail_weight, na.rm = TRUE),
            consumer_weight = sum(consumer_weight, na.rm = TRUE)) %>%
  mutate(Food = 'prepared food')

all_lafa_rates <- bind_rows(lafa_df %>% select(Food, contains('loss'), contains('weight')),
                            lafa_group_rates %>% rename(Food = subgroup),
                            overall_mean)


# Map mass flows to LAFA, convert back to $ -------------------------------

# Use the same pipe as last time to spread out the LAFA names over the rows

food_U_LAFA_spread <- food_U_LAFA %>%
  group_by(BEA_389_code, BEA_recipient_code, QFAHPD_code, price) %>%
  group_modify(~ data.frame(LAFA_name = .$LAFA_names[[1]], 
                            mass_flow = .$mass_flow/length(.$LAFA_names[[1]]),
                            monetary_flow = .$monetary_flow/length(.$LAFA_names[[1]]))) %>%
  left_join(all_lafa_rates, by = c('LAFA_name' = 'Food'))

# Save all results --------------------------------------------------------

write_csv(all_lafa_rates, file.path(fp, 'scenario_inputdata/lafa_rates_with_groups.csv'))

save(food_U_QFAHPD, food_U_LAFA, food_U_LAFA_spread, file = file.path(fp, 'crossreference_tables/intermediate_lafa_qfahpd_bea_tables.RData'))


# Table with all waste rates by BEA ---------------------------------------

# This is the most important table - convert the LAFA rates to BEA.
# The final result should be a data frame where each row is a BEA code with the 3 waste rates for the different stages.

# Get mass-weighted waste rate for each QFAHPD code by calculating the weighted average of the LAFA groups that make it up.
# QFAHPD LAFA 
qfahpd_waste_rates <- qfahpd2lafa %>% 
  unnest(LAFA_names) %>%
  left_join(all_lafa_rates, by = c('LAFA_names' = 'Food')) %>%
  group_by(QFAHPD_code, QFAHPD_name) %>%
  summarize(primary_loss = weighted.mean(x = primary_loss, w = primary_weight, na.rm = TRUE),
            retail_loss = weighted.mean(x = retail_loss, w = retail_weight, na.rm = TRUE),
            avoidable_consumer_loss = weighted.mean(x = avoidable_consumer_loss, w = consumer_weight, na.rm = TRUE))

# For going from QFAHPD to BEA, we don't really have a good weighting so we must use the unweighted averages
# Weighted by the relative prices but not the relative quantities.
qfahpd_waste_rates_prices <- qfahpd_agg %>%
  mutate(QFAHPD_code = as.numeric(QFAHPD_code)) %>%
  rename(QFAHPD_name = foodgroup) %>%
  left_join(qfahpd_waste_rates)

bea_waste_rates <- bea2qfahpd %>%
  unnest(QFAHPD_code) %>%
  mutate(QFAHPD_code = as.numeric(QFAHPD_code)) %>%
  left_join(qfahpd_waste_rates_prices) %>%
  group_by(BEA_389_code, BEA_389_def) %>%
  summarize(primary_loss_mass = mean(primary_loss),
            primary_loss_value = 100 * mean(primary_loss/100 * price)/mean(price),
            retail_loss_mass = mean(retail_loss),
            retail_loss_value = 100 * mean(retail_loss/100 * price)/mean(price),
            avoidable_consumer_loss_mass = mean(avoidable_consumer_loss),
            avoidable_consumer_loss_value = 100 * mean(avoidable_consumer_loss/100 * price)/mean(price)) %>%
  filter(!is.na(primary_loss_mass)) # filter to get rid of beverages

# quickly create diagnostic ggplot

ggplot(bea_waste_rates, aes(x = primary_loss_mass, y = primary_loss_value)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + theme_minimal()

ggplot(bea_waste_rates, aes(x = retail_loss_mass, y = retail_loss_value)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + theme_minimal()

ggplot(bea_waste_rates, aes(x = avoidable_consumer_loss_mass, y = avoidable_consumer_loss_value)) +
  geom_abline(slope = 1, linetype = 'dashed') +
  geom_point() + theme_minimal()

write_csv(qfahpd_waste_rates_prices %>% select(-LAFA_names), file.path(fp, 'crossreference_tables/waste_rates_qfahpd.csv'))
write_csv(bea_waste_rates, file.path(fp, 'crossreference_tables/waste_rates_bea.csv'))
