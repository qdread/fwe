# Tables for Food Policy MS
# QDR FWE 20 Aug 2019

# To be exported to excel and formatted there.
library(dplyr)

# Table 1. Loss rates for each stage and food type.
faotab <- read.csv('~/google_drive/SESYNC Food Waste/Model_MS1/fao_percentages_extended.csv', check.names = FALSE)
faotab <- faotab %>%
  mutate(processing = 1 - (1-`handling and storage`)*(1-`processing and packaging`)) %>%
  rename(production = `agricultural production`,
         retail = `distribution and retail`) %>%
  select(category, production, processing, retail, consumption) %>%
  rowwise %>% 
  mutate(total = 1 - prod(1-na.omit(c(production, processing, retail, consumption))))
write.csv(faotab, file = '~/google_drive/SESYNC Food Waste/Model_MS1/table1.csv', row.names = FALSE)

# Table 2. Sensitivity results. Move this to supplement I think.
meanrank_allcats <- read.csv('Q:/scenario_results/sensitivity_grid_summarytable.csv')
meanrank_allcats$proportion_not_swapped <- paste0(meanrank_allcats$proportion_not_swapped, '%')
names(meanrank_allcats) <- c('Category', 'Stage', 'Mean Rank', 'Percent Not Swapped')
write.csv(meanrank_allcats, file = '~/google_drive/SESYNC Food Waste/Model_MS1/supplementaltable2.csv', row.names = FALSE)

# Supplemental tables 1 and 2, or just 1 supplemental table: Proportion FSC and category weights for each BEA code that is >0 FSC.
cw <- read.csv('Q:/crossreference_tables/naics_crosswalk_final.csv', stringsAsFactors = FALSE)
cw$stage[cw$stage == 'agriculture'] <- 'production'

cw_out <- cw %>% select(BEA_389_code, BEA_389_def, stage, proportion_food, cereals:beverages, source_proportion_food, source_food_weighting)
write.csv(cw_out, file = '~/google_drive/SESYNC Food Waste/Model_MS1/supplementaltable1.csv', row.names = FALSE)
