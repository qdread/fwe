# Tables/figures comparing all interventions
# QDR / FWE / 12 Mar 2020

# Compare four interventions: waste tracking & analytics, spoilage prevention packaging, standardized date labeling, and consumer education campaigns
# Compare by: total cost, total environmental benefit (net and also show offset) and cost-effectiveness (cost per unit impact reduction)

# In the case of WTA, we can parse results by the foodservice sector implementing the intervention
# In the case of packaging, we can parse results by the food production/processing sector (food type) implementing the intervention
# In the case of date labeling, we have two scenarios, coordination and no coordination.
# We should compare the two separately.

# Read individual results -------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_res <- file.path(fp, 'scenario_results/interventions')

result_consumered <- read_csv(file.path(fp_res, 'eeio_consumer_ed_all.csv'))
result_datelabeling <- read_csv(file.path(fp_res, 'eeio_datelabeling_all.csv'))
result_wta <- read_csv(file.path(fp_res, 'eeio_wta_bysector_all.csv'))
result_packaging <- read_csv(file.path(fp_res, 'eeio_packaging_byfoodtype_all.csv'))
result_packaging_cost <- read_csv(file.path(fp_res, 'eeio_packaging_costperreduction_all.csv'))


# Combine into single DF --------------------------------------------------

# for packaging, add up the foods
result_packaging_combined <- result_packaging %>% 
  group_by(category) %>%
  summarize_if(is.numeric, sum) %>%
  left_join(result_packaging_cost)

# For WTA, split into a list by sector
result_wta_list <- split(result_wta, result_wta$group)

# Standardize the column names by intervention

c(list(consumered = result_consumered %>%
       select(category, impact_baseline, contains("offset"), contains("net_averted"), contains("cost_per")),
     datelabeling_coordination = result_datelabeling %>%
       select(category, impact_baseline, contains("offset"), contains("_coordination")),
     datelabeling_nocoordination = result_datelabeling %>%
       select(category, impact_baseline, contains("offset"), contains("_nocoordination")),
     ),
  result_wta_list)
