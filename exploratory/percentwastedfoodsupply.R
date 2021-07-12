# Calculation of percentage food supply wasted in USA, baseline.
# Done for Jenny Stephenson's EPA report
# QDR / 12 July 2021

library(tidyverse)

flw_rates <- read.csv('C:/Users/qread/Documents/GitHub/foodwaste/halvingfoodwaste/data/flw_rates.csv', stringsAsFactors = FALSE)

industry_proportions <- read.csv('C:/Users/qread/Documents/GitHub/foodwaste/halvingfoodwaste/data/industry_proportions.csv', stringsAsFactors = FALSE) %>%
  filter(food_system %in% c('partial', 'y')) %>%
  arrange(stage) %>%
  mutate(stage_code = case_when(
    stage %in% 'agriculture' ~ 'L1',
    stage %in% 'processing' ~ 'L2',
    stage %in% c('retail', 'wholesale') ~ 'L3',
    stage %in% 'foodservice' ~ 'L4a',
    stage %in% 'institutional' ~ 'L4b'
  ))
bea_code_formats <- read.csv('C:/Users/qread/Documents/GitHub/foodwaste/halvingfoodwaste/data/industry_codes.csv', stringsAsFactors = FALSE)

sector_stage_codes <- industry_proportions$stage_code
sector_long_names <- bea_code_formats$sector_desc_drc[match(industry_proportions$BEA_389_code, bea_code_formats$sector_code_uppercase)]
sector_short_names <- industry_proportions$BEA_389_code
final_demand_sector_codes <- sector_stage_codes
final_demand_sector_codes[final_demand_sector_codes %in% c('L1', 'L2', 'L3')] <- 'L5'

drc_base <- read.csv('C:/Users/qread/Documents/GitHub/foodwaste/halvingfoodwaste/USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_DRC.csv', check.names = FALSE, row.names = 1)

finaldemand <- read_csv("C:/Users/qread/Documents/GitHub/foodwaste/halvingfoodwaste/USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv") %>%
  left_join(industry_proportions, by = c('BEA_389_code', 'BEA_389_def')) %>% 
  mutate(proportion_food = if_else(is.na(proportion_food), 0, proportion_food))


# Baseline food system demand
f <- finaldemand$`2012_US_Consumption` * finaldemand$proportion_food

# baseline DRC
A <- as.matrix(drc_base)

# baseline total food system demand
total_base <- solve(diag(nrow(A)) - A) %*% f

# Alternative scenario where all waste is zero
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

flw_rates <- flw_rates %>%
  mutate(L1 = loss_ag_production,
         L2 = 1 - (1 - loss_handling_storage) * (1 - loss_processing_packaging),
         L3 = loss_distribution,
         L4a = loss_consumption,
         L4b = loss_consumption)

waste_rate_bysector <- t(flw_rates[, industry_proportions$stage_code])
food_category_weights <- industry_proportions %>% select(cereals:beverages)
baseline_waste_rate <- rowSums(waste_rate_bysector * food_category_weights, na.rm = TRUE) / rowSums(food_category_weights)

reduction_by_stage <- setNames(rep(1, 6), c('L1','L2','L3','L4a','L4b','L5'))
intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[sector_stage_codes], industry_proportions$proportion_food))
final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[final_demand_sector_codes], industry_proportions$proportion_food))

change_factors <- data.frame(BEA_389_code = sector_short_names,
                             intermediate_factor = intermediate_demand_change_factors,
                             final_factor = final_demand_change_factors)

finaldemand <- finaldemand %>% 
  left_join(change_factors) %>%
  replace_na(list(intermediate_factor = 0, final_factor = 0))

# Alternative food system demand
f_alt <- finaldemand$`2012_US_Consumption` * finaldemand$proportion_food * finaldemand$final_factor

# Alternative DRC
A_alt <- as.matrix(drc_base)
A_alt[, sector_long_names] <- sweep(A_alt[, sector_long_names], 2, intermediate_demand_change_factors, '*')

# Alternative total food system demand
total_nowaste <- solve(diag(nrow(A_alt)) - A_alt) %*% f_alt

# Result
idx <- finaldemand$stage %in% c('agriculture')
(sum(total_base[idx]) - sum(total_nowaste[idx])) / sum(total_base[idx])
