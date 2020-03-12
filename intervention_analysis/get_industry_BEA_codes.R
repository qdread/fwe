# Get BEA codes for the NAICS industries that may be involved in interventions
# QDR / FWE / 20 Feb 2020

library(tidyverse)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')

BEA_NAICS <- read_csv(file.path(fp, 'crossreference_tables/BEA_NAICS07_NAICS12_crosswalk.csv'))
industries <- read_csv(file.path(fp, 'scenario_inputdata/intervention_industry_NAICS.csv'))

table(industries$NAICS %in% BEA_NAICS$related_2012_NAICS_6digit) # OK

industries_joined <- BEA_NAICS %>% 
  select(BEA_Code, BEA_Title, related_2012_NAICS_6digit) %>% 
  rename(NAICS = related_2012_NAICS_6digit) %>%
  filter(!duplicated(.)) %>%
  right_join(industries)

industries_joined %>% group_by(NAICS) %>% summarize(nbea = length(unique(BEA_Code))) %>% arrange(-nbea)

BEA_NAICS %>% filter(BEA_Code %in% c('33329A', '333220')) # There is one case where a single NAICS code is related to two BEA codes
# I think they are both valid. OK.

# Get only the BEA codes, ignoring NAICS

industries_BEA <- industries_joined %>%
  select(-NAICS, -description) %>%
  filter(!duplicated(.))

industries_BEA %>% print(n=nrow(.))

write_csv(industries_joined, file.path(fp, 'scenario_inputdata/intervention_industry_NAICS_BEA.csv'))
write_csv(industries_BEA, file.path(fp, 'scenario_inputdata/intervention_industry_BEA.csv'))
