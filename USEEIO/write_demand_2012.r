# Create additional final demand columns for different USEEIO scenarios
# Uses 2012 data as baseline
# QDR/FWE/14 Nov 2018

demand <- read.csv('C:/Users/qread/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv', stringsAsFactors = FALSE, check.names = FALSE)


demand[grep('fruit|vegetable', tolower(demand$BEA_389_def)),] # This includes vegetable oils as well.

fruit_veg_codes <- c(fresh_veg = 111200, fresh_fruit = 111300, preserved_fruit_veg = 311420)

orig_demand <- demand$`2012_US_Consumption`[match(fruit_veg_codes, demand$BEA_389_code)]


# Scenario 2: Adjust the increase in canned and frozen food from scenario 2 slightly downward
# This is because demand should be less since less food is wasted.
# Use the 1.67 multiplication for price (homescan), but use ratios of waste amounts for fruit and vegetable.

source('USEEIO/lafa_fruitveg_loss.r')

processed_fresh_price_ratio <- 1.67

demand_reduction_freshfruit <- orig_demand[1] * 0.5
demand_reduction_freshveg <- orig_demand[2] * 0.5
demand_increase_cannedfruit <- demand_reduction_freshfruit * processed_fresh_price_ratio * nonfresh_fruit_loss/fresh_fruit_loss # fruit waste ratio is 0.54
demand_increase_cannedveg <- demand_reduction_freshveg * processed_fresh_price_ratio * nonfresh_veg_loss/fresh_veg_loss # veg waste ratio is 1.12

demand_change <- c(-demand_reduction_freshfruit, -demand_reduction_freshveg, demand_increase_cannedfruit + demand_increase_cannedveg)
# Original demand for fresh and preserved produce in US in 2012 (converted to 2013 dollars) was $66.2B
# This alternative scenario increases it by $7.9B to $74.0B

demand$scenario2 <- demand$`2012_US_Consumption`
demand$scenario2[match(fruit_veg_codes, demand$BEA_389_code)] <- orig_demand + demand_change
demand$scenario2_direct <- demand$scenario2
demand$scenario2_direct[!demand$BEA_389_code %in% fruit_veg_codes] <- 0
demand$baseline_direct <- demand$`2012_US_Consumption`
demand$baseline_direct[!demand$BEA_389_code %in% fruit_veg_codes] <- 0

demand <- demand %>%
  mutate(baseline_freshfruit = if_else(BEA_389_code == fruit_veg_codes['fresh_fruit'], baseline_direct, 0),
         baseline_freshveg = if_else(BEA_389_code == fruit_veg_codes['fresh_veg'], baseline_direct, 0),
         baseline_cannedfruitveg = if_else(BEA_389_code == fruit_veg_codes['preserved_fruit_veg'], baseline_direct, 0),
         scenario2_freshfruit = if_else(BEA_389_code == fruit_veg_codes['fresh_fruit'], scenario2_direct, 0),
         scenario2_freshveg = if_else(BEA_389_code == fruit_veg_codes['fresh_veg'], scenario2_direct, 0),
         scenario2_cannedfruitveg = if_else(BEA_389_code == fruit_veg_codes['preserved_fruit_veg'], scenario2_direct, 0))

write.csv(demand, '~/Dropbox/projects/foodwaste/data/demand_scenarios_2012.csv', row.names = FALSE)

