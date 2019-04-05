# Make some visualizations of the consumer expenditure data
# QDR / FWE / 08 Mar 2019

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fpcex <- file.path(fp, 'consumer_data/CEX/final_data')

# Load/clean data ---------------------------------------------------------

cex_byregion <- read.csv(file.path(fpcex, 'cex_region.csv'), stringsAsFactors = FALSE)
cex_byincome <- read.csv(file.path(fpcex, 'cex_incomeclass.csv'), stringsAsFactors = FALSE)

# Find duplicates
isdup <- duplicated(cex_byregion[,1:4])

# Reshape the data so that mean and se have their own columns, and the groups have their own rows
# Make sure order is preserved so that it's easier to look at the data in the intended ordertmp <- cex_byregion %>% 
cex_byregion <- cex_byregion %>% 
  mutate(id = paste(group, title)) 

cex_byregiongrp <- cex_byregion %>%
  group_by(year, group, estimate) %>%
  filter(!duplicated(title)) %>%
  ungroup %>%
  gather(region, value, -id, -year, -title, -group, -estimate) %>%
  group_by(year, id, title, group, region) %>%
  spread(estimate, value)

cex_byregiongrp <- cex_byregiongrp[order(match(cex_byregiongrp$id, cex_byregion$id)), ] %>%
  ungroup %>%
  select(-id)

cex_byincome <- cex_byincome %>% 
  mutate(id = paste(group, title)) 

cex_byincomegrp <- cex_byincome %>%
  select(-level) %>%
  group_by(year, group, estimate) %>%
  filter(!duplicated(title)) %>%
  ungroup %>%
  gather(income, value, -id, -year, -title, -group, -estimate) %>%
  group_by(year, id, title, group, income) %>%
  spread(estimate, value)

cex_byincomegrp <- cex_byincomegrp[order(match(cex_byincomegrp$id, cex_byincome$id)), ] %>%
  ungroup %>%
  select(-id)


# Basic data viz ----------------------------------------------------------

fpfig <- ifelse(dir.exists('Q:/'), 'Q:/figures', '/nfs/qread-data/figures')

# For now just look at US wide data
cex_all <- cex_byregiongrp %>%
  filter(grepl('All', region)) %>%
  ungroup %>%
  select(-region)

exp_all <- cex_all %>% filter(group %in% 'EXPEND')

# total annual expenditures
exp_all %>%
  filter(title == 'Average annual expenditures') %>%
  ggplot(aes(x = factor(year), y = mn, ymin = mn - se, ymax = mn + se)) +
    geom_errorbar(width = 0.2) +
    geom_point() +
    theme_bw() +
    scale_y_continuous(name = 'Expenditure', labels = scales::dollar) +
    labs(x = 'Year') +
    ggtitle('Total annual household expenditures')
ggsave(file.path(fpfig, 'cex_totalexp.png'), height = 4, width = 6, dpi = 300)

# total expenditures by income and region
cex_byregiongrp %>%
  filter(title == 'Average annual expenditures', !region %in% 'All.CU.s') %>%
  ggplot(aes(x = factor(year), color = region, y = mn, ymin = mn - se, ymax = mn + se)) +
    geom_errorbar(width = 0.2) +
    geom_point() +
    theme_bw() +
    scale_y_continuous(name = 'Expenditure', labels = scales::dollar) +
    labs(x = 'Year') +
    ggtitle('Total annual household expenditures')
ggsave(file.path(fpfig, 'cex_totalexp_region.png'), height = 4, width = 7, dpi = 300)

cex_byincomegrp %>%
  filter(title == 'Average annual expenditures', !income %in% 'All.CU.s', year == 2012) %>%
  ggplot(aes(x = factor(income), y = mn, ymin = mn - se, ymax = mn + se)) +
    geom_errorbar(width = 0.2) +
    geom_point() +
    theme_bw() +
    scale_y_continuous(name = 'Expenditure', labels = scales::dollar) +
    scale_x_discrete(name = 'Income class', labels = 1:9) +
    ggtitle('Total annual household expenditures, 2012')
ggsave(file.path(fpfig, 'cex_totalexp_income2012.png'), height = 4, width = 6, dpi = 300)

# Food at home and away from home

# First need to add Food at home and Other food at home (still small discrepancy but prob OK)
fah1 <- cex_all %>% filter(title %in% 'Food at home')
fah2 <- cex_all %>% filter(title %in% 'Other food at home')

fah1$mn <- fah1$mn + fah2$mn
fah1$se <- sqrt((fah1$se)^2 + (fah2$se)^2)

fah_and_fafh <- cex_all %>%
  filter(title %in% 'Food away from home') %>%
  rbind(fah1)

ggplot(fah_and_fafh, aes(x = factor(year), color = title, y = mn, ymin = mn - se, ymax = mn + se)) +
  geom_errorbar(width = 0.2) +
  geom_point() +
  theme_bw() +
  theme(legend.position = c(0.2, 0.2), legend.title = element_blank()) +
  scale_y_continuous(name = 'Expenditure', labels = scales::dollar, limits = c(0, 4500), expand = c(0,0)) +
  labs(x = 'Year') +
  ggtitle('Total annual food expenditures')
ggsave(file.path(fpfig, 'cex_foodexp.png'), height = 4, width = 6, dpi = 300)


# Food at home and away from home by region and by income


fah_byincome <- cex_byincomegrp %>%
  filter(title %in% c('Food at home', 'Other food at home')) %>%
  group_by(year, income) %>%
  summarize(mn = sum(mn), se = sqrt(sum(se^2))) %>%
  ungroup %>%
  mutate(title = 'Food at home')

fah_fafh_byincome <- cex_byincomegrp %>%
  filter(title %in% c('Food away from home')) %>%
  bind_rows(fah_byincome)
  

fah_byregion <- cex_byregiongrp %>%
  filter(title %in% c('Food at home', 'Other food at home')) %>%
  group_by(year, region) %>%
  summarize(mn = sum(mn), se = sqrt(sum(se^2))) %>%
  ungroup %>%
  mutate(title = 'Food at home')

fah_fafh_byregion <- cex_byregiongrp %>%
  filter(title %in% c('Food away from home')) %>%
  bind_rows(fah_byregion)

ggplot(fah_fafh_byincome %>% filter(year == 2012, !income %in% 'All.CU.s'), aes(x = factor(income), y = mn, ymin = mn - se, ymax = mn + se, color = title)) +
  geom_errorbar(width = 0.2) +
  geom_point() +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7), legend.title = element_blank()) +
  scale_y_continuous(name = 'Expenditure', labels = scales::dollar) +
  scale_x_discrete(name = 'Income class', labels = 1:9) +
  ggtitle('Total annual food expenditures, 2012')
ggsave(file.path(fpfig, 'cex_foodexp_income2012.png'), height = 4, width = 6, dpi = 300)

ggplot(fah_fafh_byregion %>% filter(year == 2012, !region %in% 'All.CU.s'), aes(x = factor(region), y = mn, ymin = mn - se, ymax = mn + se, color = title)) +
  geom_errorbar(width = 0.2) +
  geom_point() +
  theme_bw() +
  theme(legend.position = c(0.2, 0.2), legend.title = element_blank()) +
  scale_y_continuous(name = 'Expenditure', labels = scales::dollar, limits = c(0, 4500), expand = c(0, 0)) +
  scale_x_discrete(name = 'Region') +
  ggtitle('Total annual food expenditures, 2012')
ggsave(file.path(fpfig, 'cex_totalexp_region2012.png'), height = 4, width = 6, dpi = 300)


# Classify certain groups as food-related ---------------------------------

#write.csv(unique(cex_all[,c('title','group')]), file = '~/Dropbox/projects/foodwaste/Data/cex_title_lookup.csv', row.names = FALSE)
foodlookup <- read.csv('~/Dropbox/projects/foodwaste/Data/cex_title_lookup.csv', stringsAsFactors = FALSE)

# Pull the information for the "food related" groups
cex_all <- cex_all %>%
  left_join(foodlookup)

# List relevant categories
util_fuel_cats <- c('Plumbing and water heating', 'Natural gas', 'Electricity', 'Fuel oil and other fuels', 'Bottled gas', 'Water and sewerage maintenance', 'Trash and garbage collection')
household_cats <- c('Soaps and detergents', 'Dishwashers (built-in), garbage disposals, range hoods, (renter)', 'Dishwashers (built-in), garbage disposals, range hoods, (owned home)', 'Refrigerators, freezers (renter)', 'Refrigerators, freezers (owned home)', 'Cooking stoves, ovens (renter)', 'Cooking stoves, ovens (owned home)', 'Microwave ovens (renter)', 'Microwave ovens (owned home)', 'Portable dishwasher (renter)', 'Portable dishwasher (owned home)', 'Small appliances, miscellaneous housewares', 'Small electric kitchen appliances')
travel_cats <- c('Gasoline', 'Diesel fuel', 'Intracity mass transit fares')

# Different groups to be at least partially attributed to food.
income_util <- cex_byincomegrp %>% filter(title %in% util_fuel_cats, group %in% 'EXPEND')
income_hhold <- cex_byincomegrp %>% filter(title %in% household_cats, group %in% 'EXPEND')
income_travel <- cex_byincomegrp %>% filter(title %in% travel_cats, group %in% 'EXPEND')

region_util <- cex_byregiongrp %>% filter(title %in% util_fuel_cats, group %in% 'EXPEND')
region_hhold <- cex_byregiongrp %>% filter(title %in% household_cats, group %in% 'EXPEND')
region_travel <- cex_byregiongrp %>% filter(title %in% travel_cats, group %in% 'EXPEND')

expend_byincome <- map2_dfr(c('utilities','household','travel'), list(income_util, income_hhold, income_travel), ~ data.frame(subcategory = .x, .y))
expend_byregion <- map2_dfr(c('utilities','household','travel'), list(region_util, region_hhold, region_travel), ~ data.frame(subcategory = .x, .y))

write.csv(expend_byincome, file.path(fp, 'CEX/final_data/selectedexpenses_income.csv'), row.names = FALSE)
write.csv(expend_byregion, file.path(fp, 'CEX/final_data/selectedexpenses_region.csv'), row.names = FALSE)


# Data viz of expenditures ------------------------------------------------

expend_byincome <- read.csv(file.path(fpcex, 'selectedexpenses_income.csv'), stringsAsFactors = FALSE)
expend_byregion <- read.csv(file.path(fpcex, 'selectedexpenses_region.csv'), stringsAsFactors = FALSE)

brewfill <- scale_fill_brewer(type = 'qual', palette = 'Set1')

ggplot(expend_byregion %>% filter(year == 2012, subcategory == 'utilities'), aes(x = region, y = mn, fill = title)) +
  geom_col(position = 'stack') +
  brewfill +
  scale_y_continuous(name = 'Expenditure', labels = scales::dollar, limits =c(0, 2750), expand = c(0, 0)) +
  scale_x_discrete(name = 'Region', labels = c('All', 'Midwest', 'Northeast', 'South', 'West')) +
  theme_bw() +
  ggtitle('Average annual utility expenditures, 2012')
ggsave(file.path(fpfig, 'cex_utilityexp_region2012.png'), height = 4, width = 7, dpi = 300)


# Sum up owned home and renter categories to reduce the number of categories before doing "household"
# Remove anything in parentheses at the end
expend_byregion_household <- expend_byregion %>%
  filter(subcategory == 'household') %>%
  mutate(title = gsub('\\([^()]+\\)$', '', title),
         title = gsub(',$', '', trimws(title))) %>%
  group_by(year, title, region) %>%
  summarize(mn = sum(mn),
            se = sqrt(sum(se^2)))

ggplot(expend_byregion_household %>% filter(year == 2012), aes(x = region, y = mn, fill = title)) +
  geom_col(position = 'stack') +
  brewfill +
  scale_y_continuous(name = 'Expenditure', labels = scales::dollar, limits =c(0, 400), expand = c(0, 0)) +
  scale_x_discrete(name = 'Region', labels = c('All', 'Midwest', 'Northeast', 'South', 'West')) +
  theme_bw() +
  ggtitle('Average annual kitchen-related expenditures, 2012')
ggsave(file.path(fpfig, 'cex_kitchenexp_region2012.png'), height = 4, width = 7, dpi = 300)

ggplot(expend_byregion %>% filter(year == 2012, subcategory == 'travel'), aes(x = region, y = mn, fill = title)) +
  geom_col(position = 'stack') +
  brewfill +
  scale_y_continuous(name = 'Expenditure', labels = scales::dollar, limits =c(0, 3000), expand = c(0, 0)) +
  scale_x_discrete(name = 'Region', labels = c('All', 'Midwest', 'Northeast', 'South', 'West')) +
  theme_bw() +
  ggtitle('Average annual local travel expenditures, 2012')
ggsave(file.path(fpfig, 'cex_localtravelexp_region2012.png'), height = 4, width = 7, dpi = 300)
