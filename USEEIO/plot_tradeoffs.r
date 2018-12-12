# Plot tradeoffs with the different scenarios for increased usage of plastic film
# QDR / FWE / 10 Dec 2018

scen_dat <- read.csv('Q:/scenario_results/plasticfilm_forfruitandveg.csv')

library(dplyr)
library(ggplot2)

# Plot the tradeoff of greenhouse gas emissions

scen_dat %>%
  filter(impact_category == "impact potential/gcc/kg co2 eq") %>%
  ggplot(aes(x = plastic_demand_increase, y = Total, group = factor(fruitveg_demand_decrease), color = factor(fruitveg_demand_decrease))) +
    geom_line()

scen_dat %>%
  filter(impact_category == "resource use/enrg/mj") %>%
  ggplot(aes(x = plastic_demand_increase, y = Total, group = factor(fruitveg_demand_decrease), color = factor(fruitveg_demand_decrease))) +
  geom_line()

scen_dat %>%
  filter(impact_category == "resource use/mine/kg") %>%
  ggplot(aes(x = plastic_demand_increase, y = Total, group = factor(fruitveg_demand_decrease), color = factor(fruitveg_demand_decrease))) +
  geom_line()

scen_dat %>%
  filter(impact_category == "resource use/land/m2*yr") %>%
  ggplot(aes(x = plastic_demand_increase, y = Total, group = factor(fruitveg_demand_decrease), color = factor(fruitveg_demand_decrease))) +
  geom_line()
