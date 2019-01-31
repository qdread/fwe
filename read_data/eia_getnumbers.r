# Get totals from EIA data
# QDR FWE 29 Jan 2019

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')

fuel <- read.csv(file.path(fp, 'EIA/processed/fuel_used_usa.csv'), stringsAsFactors = FALSE)
appl <- read.csv(file.path(fp, 'EIA/processed/appliances_usa.csv'), stringsAsFactors = FALSE)
heat <- read.csv(file.path(fp, 'EIA/processed/waterheating_usa.csv'), stringsAsFactors = FALSE)

fuel_usa <- fuel %>% 
  filter(grepl('Total', group1),
         is.na(group4),
         !grepl('All', group2)) %>%
  select(group2, group3, data, rse, flag) %>%
  arrange(group2, group3) %>%
  rename(fuel = group2, use = group3)

fpload <- file.path(fp, 'EIA/processed')

ce_summ <- read.csv(file.path(fpload, 'EIA_consumption_expenditure_summarystats.csv'), stringsAsFactors = FALSE)
e_byfuel <- read.csv(file.path(fpload, 'EIA_expenditure_byfuel.csv'), stringsAsFactors = FALSE)
e_byuse <- read.csv(file.path(fpload, 'EIA_expenditure_byuse.csv'), stringsAsFactors = FALSE)
e_byfuelxuse <- read.csv(file.path(fpload, 'EIA_expenditure_byfuelXuse.csv'), stringsAsFactors = FALSE)
e_detail_elec <- read.csv(file.path(fpload, 'EIA_expenditure_detailed_electricity.csv'), stringsAsFactors = FALSE)
e_detail_gas <- read.csv(file.path(fpload, 'EIA_expenditure_detailed_naturalgas.csv'), stringsAsFactors = FALSE)

kitchen_categories <- c('Cooking', 'Dishwashers', 'Microwaves', 'All refrigerators', 'Separate freezers')
duplicate_categories <- c('Mostused refrigerators', 'Mostused TVs', 'Second refrigerators', 'Second TVs')
elec_usa <- e_detail_elec %>%
  filter(group1 %in% 'All homes') %>%
  mutate(header1 = gsub('-', '', header1),
         kitchen = header1 %in% kitchen_categories) %>%
  filter(!header1 %in% duplicate_categories)

elec_usa %>% group_by(kitchen) %>% summarize(dollars = sum(dollars)) %>% ungroup %>% mutate(pct = dollars/sum(dollars))

natgas_usa <- e_detail_gas %>%
  filter(group1 %in% 'All homes') %>%
  mutate(kitchen = header2 %in% kitchen_categories)

natgas_usa %>% group_by(kitchen) %>% summarize(dollars = sum(dollars)) %>% ungroup %>% mutate(pct = dollars/sum(dollars))


# Make overall plots ------------------------------------------------------

# Plot expenditures on electricity by type as a (gasp) pie chart
ggplot(elec_usa, aes(x = header1, y = dollars, fill = kitchen)) + geom_col()
pie_elec_overall <- ggplot(elec_usa %>% group_by(kitchen) %>% summarize(dollars = sum(dollars)), aes(x = 1, y = dollars, fill = kitchen)) + 
  geom_col(position = 'stack') + 
  coord_polar(theta = 'y') + 
  theme_void() +
  theme(legend.position = 'none') +
  annotate('text', x = c(1.1, 1), y = c(10, 70), label = c('Food-related\n14% ($19.9B)','Not food-related\n86% ($121B)')) +
  ggtitle('Household electricity expenditure 2015', 'source: EIA Residential Energy Consumption Survey')

# Breakdown of pie slice by type
elec_usa_kitchen <- elec_usa %>% filter(kitchen)

pie_elec_byuse <- ggplot(elec_usa_kitchen, aes(x = 1, y = dollars, fill = header1)) +
  geom_col(position = 'stack') + 
  coord_polar(theta = 'y') + 
  theme_void() +
  annotate('text', x = 1, y = c(1.3, 2.61+0.95, 2.61+1.89+0.48, 2.61+1.89+0.96+1.17, 2.61+1.89+0.96+2.34+6), label = paste0('$', round(rev(elec_usa_kitchen$dollars), 1), 'B')) +
  scale_fill_discrete(name = 'End use') +
  ggtitle('Household food-related electricity expenditure 2015', 'source: EIA Residential Energy Consumption Survey')
  
# natural gas and propane, added up
pie_natgas_overall <- natgas_usa %>% group_by(kitchen) %>% summarize(dollars = sum(dollars)) %>%
  ggplot(aes(x = 1, y = dollars, fill = kitchen)) +
    geom_col(position = 'stack') + 
    coord_polar(theta = 'y') + 
    theme_void() +
    theme(legend.position = 'none') +
    annotate('text', x = c(1, 1), y = c(1, 23), label = c('Cooking\n4% ($1.8B)','Not cooking\n96% ($45.1B)')) +
    ggtitle('Household natural gas/propane expenditure 2015', 'source: EIA Residential Energy Consumption Survey')


# Make plots by tabulated groups ------------------------------------------

# Does the % of energy expenditure on food have a relationship to: income group, census region, climate region, housing unit type?

elec_by_censusregion <- e_detail_elec %>%
  filter(group1 %in% 'Census region and division') %>%
  mutate(header1 = gsub('-', '', header1)) %>%
  filter(!header1 %in% duplicate_categories) %>%
  group_by(group2, header1) %>%
  summarize(dollars = sum(dollars, na.rm = TRUE)) %>%
  mutate(kitchen = header1 %in% kitchen_categories)

col_elec_bycensus <- elec_by_censusregion %>% 
  group_by(group2, kitchen) %>% 
  summarize(dollars = sum(dollars, na.rm = TRUE)) %>%
  mutate(pct = dollars/sum(dollars)) %>%
  ungroup %>%
  mutate(group2 = factor(group2, levels = c('South','West','Midwest','Northeast'))) %>%
  ggplot(aes(x = group2, y = dollars, fill = kitchen)) +
    geom_col(position = 'stack') +
    geom_text(aes(label = paste0(round(pct, 2) * 100, '%')), vjust = 1) +
    scale_fill_discrete(name = 'End use', labels = c('Not food-related', 'Food-related')) +
    labs(x = 'Census region', y = 'Expenditure (billion $)') +
    theme_minimal() +
    theme(legend.position = c(0.8, 0.8)) +
    ggtitle('Household electricity expenditure 2015, by census region', 'source: EIA Residential Energy Consumption Survey')

elec_by_climateregion <- e_detail_elec %>%
  filter(group1 %in% 'Climate region') %>%
  mutate(header1 = gsub('-', '', header1)) %>%
  filter(!header1 %in% duplicate_categories) %>%
  group_by(group2, header1) %>%
  summarize(dollars = sum(dollars, na.rm = TRUE)) %>%
  mutate(kitchen = header1 %in% kitchen_categories)

col_elec_byclimate <- elec_by_climateregion %>% 
  group_by(group2, kitchen) %>% 
  summarize(dollars = sum(dollars, na.rm = TRUE)) %>%
  mutate(pct = dollars/sum(dollars)) %>%
  ungroup %>%
  mutate(group2 = factor(group2, levels = c('Very cold/Cold', 'Marine', 'Mixed-humid', 'Mixed-dry/Hot-dry', 'Hot-humid'))) %>%
  ggplot(aes(x = group2, y = dollars, fill = kitchen)) +
  geom_col(position = 'stack') +
  geom_text(aes(label = paste0(round(pct, 2) * 100, '%')), vjust = 1) +
  scale_fill_discrete(name = 'End use', labels = c('Not food-related', 'Food-related')) +
  labs(x = 'Climate region', y = 'Expenditure (billion $)') +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.85)) +
  ggtitle('Household electricity expenditure 2015, by climate region', 'source: EIA Residential Energy Consumption Survey')

elec_by_income <- e_detail_elec %>%
  filter(group1 %in% '2015 annual household income') %>%
  mutate(header1 = gsub('-', '', header1)) %>%
  filter(!header1 %in% duplicate_categories) %>%
  group_by(group2, header1) %>%
  summarize(dollars = sum(dollars, na.rm = TRUE)) %>%
  mutate(kitchen = header1 %in% kitchen_categories)

sorted_income <- unique(elec_by_income$group2)[c(8, 4, 5, 6, 7, 1, 2, 3)]
sorted_income_labels <- c('<$20K', '$20K-$40K', '$40K-$60K', '$60K-$80K', '$80K-$100K', '$100K-$120K', '$120K-$140K', '>$140K')

col_elec_byincome <- elec_by_income %>% 
  group_by(group2, kitchen) %>% 
  summarize(dollars = sum(dollars, na.rm = TRUE)) %>%
  mutate(pct = dollars/sum(dollars)) %>%
  ungroup %>%
  mutate(group2 = factor(group2, levels = sorted_income, labels = sorted_income_labels)) %>%
  ggplot(aes(x = group2, y = dollars, fill = kitchen)) +
  geom_col(position = 'stack') +
  geom_text(aes(label = paste0(round(pct, 2) * 100, '%')), vjust = 1) +
  scale_fill_discrete(name = 'End use', labels = c('Not food-related', 'Food-related')) +
  labs(x = 'Income group', y = 'Expenditure (billion $)') +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.85)) +
  ggtitle('Household electricity expenditure 2015, by income group', 'source: EIA Residential Energy Consumption Survey')

elec_by_housing <- e_detail_elec %>%
  filter(group1 %in% 'Housing unit type') %>%
  mutate(header1 = gsub('-', '', header1)) %>%
  filter(!header1 %in% duplicate_categories) %>%
  group_by(group2, header1) %>%
  summarize(dollars = sum(dollars, na.rm = TRUE)) %>%
  mutate(kitchen = header1 %in% kitchen_categories)

sorted_housing <- unique(elec_by_housing$group2)[c(5, 4, 3, 1, 2)]
sorted_housing_labels <- c('Single-family\ndetached', 'Single-family\nattached', 'Mobile home', 'Apt. in\nsmall bldg.', 'Apt. in \nlarge bldg.')

col_elec_byhousing <- elec_by_housing %>% 
  group_by(group2, kitchen) %>% 
  summarize(dollars = sum(dollars, na.rm = TRUE)) %>%
  mutate(pct = dollars/sum(dollars)) %>%
  ungroup %>%
  mutate(group2 = factor(group2, levels = sorted_housing, labels = sorted_housing_labels)) %>%
  ggplot(aes(x = group2, y = dollars, fill = kitchen)) +
  geom_col(position = 'stack') +
  geom_text(aes(label = paste0(round(pct, 2) * 100, '%')), vjust = 1) +
  scale_fill_discrete(name = 'End use', labels = c('Not food-related', 'Food-related')) +
  labs(x = 'Income group', y = 'Expenditure (billion $)') +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.85)) +
  ggtitle('Household electricity expenditure 2015, by housing unit type', 'source: EIA Residential Energy Consumption Survey')


# Write plots -------------------------------------------------------------

pdf('~/google_drive/SESYNC Food Waste/Results_Figures/household_energy_use.pdf', height = 6, width = 6)
  print(pie_elec_overall)
  print(pie_elec_byuse)
  print(pie_natgas_overall)
  print(col_elec_bycensus)
  print(col_elec_byclimate)
  print(col_elec_byincome + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  print(col_elec_byhousing)
dev.off()