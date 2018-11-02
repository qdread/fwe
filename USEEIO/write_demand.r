# Programatically load different components of the USEEIO data and export them to CSVs
# QDR/FWE/31 Oct 2018

library(XLConnect)

f <- '~/Dropbox/projects/foodwaste/Data/Yang_etal_2017_SI3.xlsx'
wb <- loadWorkbook(f)
getSheets(wb)

demand <- readWorksheet(wb, sheet='USEEIO_Demand')

# Create scenario with increased canned vegetable consumption and decreased fresh vegetable consumption, each by 25%

demand$scenario1 <- demand$USConsumption

demand[grep('fruit|vegetable', tolower(demand$Name)),]

orig_demand <- demand$USConsumption[grep('fruit|vegetable', tolower(demand$Name))]

# scenario 1: keep total expenditure on food constant. 
# Reduce consumption of fresh fruit and vegetable by 50% apiece.
# Increase consumption of canned fruit and vegetable by the same dollar amount. This results in 70% increase in canning of fruits and vegs.

demand_change <- c(-orig_demand[1:2] * 0.5, sum(orig_demand[1:2] * 0.5))
demand_change / orig_demand
new_demand <- orig_demand + demand_change
sum(new_demand) == sum(orig_demand) # Total is the same

demand$scenario1[grep('fruit|vegetable', tolower(demand$Name))] <- new_demand

demand[grep('fruit|vegetable', tolower(demand$Name)), ]

write.csv(demand, '~/Dropbox/projects/foodwaste/data/demand_scenarios.csv', row.names = FALSE)

# scenario 2: If consumption of fresh fruit and vegetable is decreased by a certain amount, keep number of servings constant
# Since processed fruit and vegetables cost more, the demand must increase by an even greater amount.
# assume they are 1.5x more expensive.

demand_change <- c(-orig_demand[1:2] * 0.5, sum(orig_demand[1:2] * 0.5 * 1.5))
demand_change / orig_demand

demand$scenario2 <- demand$USConsumption
demand$scenario2[grep('fruit|vegetable', tolower(demand$Name))] <- orig_demand + demand_change

write.csv(demand, '~/Dropbox/projects/foodwaste/data/demand_scenarios.csv', row.names = FALSE)

# Scenario  2 with direct only
demand$scenario2_direct <- demand$scenario2
demand$scenario2_direct[!grepl('fruit|vegetable', tolower(demand$Name))] <- 0
demand$baseline_direct <- demand$USConsumption
demand$baseline_direct[!grepl('fruit|vegetable', tolower(demand$Name))] <- 0
write.csv(demand, '~/Dropbox/projects/foodwaste/data/demand_scenarios.csv', row.names = FALSE)

# Create separate baseline and change scenarios for the three individual components so that we can parse out the waste due to each one.

demand <- demand %>%
  mutate(baseline_freshfruit = if_else(Name == 'Fruit and tree nut farming', baseline_direct, 0),
         baseline_freshveg = if_else(Name == 'Vegetable and melon farming', baseline_direct, 0),
         baseline_cannedfruitveg = if_else(Name == 'Fruit and vegetable canning, pickling, and drying', baseline_direct, 0),
         scenario2_freshfruit = if_else(Name == 'Fruit and tree nut farming', scenario2_direct, 0),
         scenario2_freshveg = if_else(Name == 'Vegetable and melon farming', scenario2_direct, 0),
         scenario2_cannedfruitveg = if_else(Name == 'Fruit and vegetable canning, pickling, and drying', scenario2_direct, 0))
write.csv(demand, '~/Dropbox/projects/foodwaste/data/demand_scenarios.csv', row.names = FALSE)


# Scenario 2 modified: Adjust the increase in canned and frozen food from scenario 2 slightly downward
# This is because demand should be less since less food is wasted.
# Use the 1.67 multiplication for price (homescan), but use ratios of waste amounts for fruit and vegetable.

processed_fresh_price_ratio <- 1.67

demand_reduction_freshfruit <- orig_demand[1] * 0.5
demand_reduction_freshveg <- orig_demand[2] * 0.5
demand_increase_cannedfruit <- demand_reduction_freshfruit * processed_fresh_price_ratio * nonfresh_fruit_loss/fresh_fruit_loss # fruit waste ratio is 0.54
demand_increase_cannedveg <- demand_reduction_freshveg * processed_fresh_price_ratio * nonfresh_veg_loss/fresh_veg_loss # veg waste ratio is 1.12

demand_change <- c(-demand_reduction_freshfruit, -demand_reduction_freshveg, demand_increase_cannedfruit + demand_increase_cannedveg)
# Original demand for fresh and preserved produce in US in 2007 was $69.4B
# This alternative scenario increases it by $7.5B to $76.9B

demand$scenario2 <- demand$USConsumption
demand$scenario2[grep('fruit|vegetable', tolower(demand$Name))] <- orig_demand + demand_change
demand$scenario2_direct <- demand$scenario2
demand$scenario2_direct[!grepl('fruit|vegetable', tolower(demand$Name))] <- 0
demand$baseline_direct <- demand$USConsumption
demand$baseline_direct[!grepl('fruit|vegetable', tolower(demand$Name))] <- 0

demand <- demand %>%
  mutate(baseline_freshfruit = if_else(Name == 'Fruit and tree nut farming', baseline_direct, 0),
         baseline_freshveg = if_else(Name == 'Vegetable and melon farming', baseline_direct, 0),
         baseline_cannedfruitveg = if_else(Name == 'Fruit and vegetable canning, pickling, and drying', baseline_direct, 0),
         scenario2_freshfruit = if_else(Name == 'Fruit and tree nut farming', scenario2_direct, 0),
         scenario2_freshveg = if_else(Name == 'Vegetable and melon farming', scenario2_direct, 0),
         scenario2_cannedfruitveg = if_else(Name == 'Fruit and vegetable canning, pickling, and drying', scenario2_direct, 0))

write.csv(demand, '~/Dropbox/projects/foodwaste/data/demand_scenarios.csv', row.names = FALSE)

