# Load intervention tables
library(XLConnect)
library(tidyverse)

intvs <- readWorksheetFromFile('~/Dropbox/projects/foodwaste/Synthesis_MS/intervention tabulation.xlsx', sheet = 1, startRow = 4)
# There are some with multiple FSC stages

intvs <- intvs %>%
  filter(!duplicated(Brief.Description)) %>%
  mutate(intervention_type = names(.)[3:5][apply(., 1, function(x) which(!is.na(x[3:5])))],
         change_type = names(.)[6:8][apply(., 1, function(x) which(!is.na(x[6:8])))],
         fsc_stage = map(apply(., 1, function(x) which(!is.na(x[9:13]))), names)) %>%
  select(Source, Brief.Description, intervention_type, change_type, fsc_stage)

intvs_long <- intvs %>% rowwise %>% do(with(., data.frame(Source, Brief.Description, intervention_type, change_type, fsc_stage))) %>%
  mutate(intervention_type = factor(intervention_type, levels = c('Prevention','Recovery','Recycling')),
         change_type = factor(change_type, levels = c('Policy', 'Technology', 'Practice.or.Behavior'), labels = c('Policy', 'Technology', 'Practice or Behavior')),
         fsc_stage = factor(fsc_stage, levels = c('Farms...Fisheries','Manufacturers','Restaurants...Retailers','Emergency.Food.Providers','Consumers'), labels = c('Farms & Fisheries','Manufacturers','Restaurants & Retailers','Emergency Food Providers','Consumers')))

intvs_tables <- intvs_long %>%
  ungroup %>%
  mutate(n = 1:nrow(.)) %>%
  group_by(intervention_type, change_type, fsc_stage) %>%
  summarize(interventions = paste(Brief.Description, collapse = '\n'))

# Regroup

intvs_tables <- intvs_tables %>%
  select(intervention_type, change_type, fsc_stage, interventions) %>%
  group_by(intervention_type, fsc_stage) %>%
  spread(change_type, interventions, fill = '') %>%
  rename(`Supply Chain Stage` = fsc_stage)

intvs_tables %>% split(.$intervention_type) %>%
  iwalk(~ writeWorksheetToFile(file = '~/Dropbox/projects/foodwaste/Synthesis_MS/sortedinterventiontable2.xlsx', data = .x[,-1], sheet = .y))

write.csv(intvs_tables, file = '~/Dropbox/projects/foodwaste/Synthesis_MS/sortedinterventiontable.csv', row.names = FALSE)
  