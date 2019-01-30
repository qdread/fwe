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

# Plot expenditures on electricity by type as a (gasp) pie chart
ggplot(elec_usa, aes(x = header1, y = dollars, fill = kitchen)) + geom_col()
