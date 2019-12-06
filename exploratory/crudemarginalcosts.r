# Very crude analysis to get marginal impact abatement for final demand reduction in "wholesale" and "retail" food

library(tidyverse)

is_local <- dir.exists('Z:/')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub', '~'))

source(file.path(fp_github, 'fwe/USEEIO/load_scenario_data.r'))
source(file.path(fp_github, 'fwe/figs/theme_black.R'))

gross_outputs <- M[naics_foodsystem$BEA_389_code, 'T008'] * naics_foodsystem$proportion_food

# Get the matching codes.
all_codes <- read.csv(file.path(fp_crosswalk, 'all_codes.csv'), stringsAsFactors = FALSE)
code_lookup <- all_codes %>%
  transmute(BEA_code = sector_code_uppercase, BEA_code_full = sector_desc_drc)

naics_foodsystem <- naics_foodsystem %>%
  left_join(code_lookup, by = c('BEA_389_code' = 'BEA_code'))

naics_foodsystem <- naics_foodsystem %>%
  mutate(gross_output = gross_outputs)

# Divide 1 unit ($1m) of demand reduction across each stage and calculate USEEIO for each one

eeio_reduce <- naics_foodsystem %>%
  group_by(stage_code) %>%
  group_map(~ {
    demand_reduced <- .$gross_output / sum(.$gross_output)
    eeio_lcia('USEEIO2012', as.list(demand_reduced), as.list(.$BEA_code_full))
  })

eeio_wholesale_retail <- naics_foodsystem %>%
  mutate(grp = if_else(stage_code %in% c('L1','L2','L3'), 'wholesale', 'retail')) %>%
  group_by(grp) %>%
  group_map(~ {
    demand_reduced <- .$gross_output / sum(.$gross_output)
    eeio_lcia('USEEIO2012', as.list(demand_reduced), as.list(.$BEA_code_full))
  })


# Crudely get cost to reduce a ton of co2 ---------------------------------

# Wholesale and retail values
v_wholesale <- eeio_wholesale_retail[[1]]["impact potential/gcc/kg co2 eq", "Total"]
v_retail <- eeio_wholesale_retail[[2]]["impact potential/gcc/kg co2 eq", "Total"]

interv_cost <- readxl::read_xlsx(file.path(fp_crosswalk, 'intervention_withcosts.xlsx'))

# Improve annualized cost. (implement annuity function as in excel, here f and t are both zero)
pmt <- function(p, r, n, f, t) (p * r * (1+r)^n  - f) / (((1+r)^n - 1) * (1 + r * t))

# Correctly annualize one time cost with 7% interest rate over ten years

annualized_onetimecost <- pmt(p = interv_cost$`One time cost`, r = 0.07, n = 10, f = 0, t = 0)

costs <- (annualized_onetimecost + interv_cost$`Annual cost`)[1:15]
avoided_values <- interv_cost$`Avoided waste value`[1:15]
wh_ret <- interv_cost$`value class`[1:15]

vs <- ifelse(wh_ret == 'wholesale', v_wholesale, v_retail)

avoided_co2 <- avoided_values * vs

net_costs <- costs - avoided_values

net_costs / (avoided_co2 / 1000)

sum(net_costs) / sum(avoided_co2 / 1000)

sum(avoided_co2 / 1000 / 1e6) # 10 million tons, 0.01 gigatons (?)


qplot(x = costs / (avoided_co2 / 1000), geom = 'histogram')

# Results as table

cost_table <- data.frame(intervention = interv_cost$Intervention[1:15],
           cost = costs,
           avoided_value = avoided_values,
           class = wh_ret,
           avoided_co2 = avoided_co2,
           net_cost = net_costs,
           cost_per_ton = costs/(avoided_co2/1000),
           net_cost_per_ton = net_costs/(avoided_co2/1000))

# Make boxplots
cost_long <- cost_table %>%
  select(intervention, cost_per_ton, net_cost_per_ton) %>%
  pivot_longer(-intervention, names_to = 'type')

ggplot(cost_long, aes(x = type, y = value)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', binwidth = 50, color = 'white', fill = 'white') +
  geom_hline(yintercept = 46, linetype = 'dotted', color = 'gray50', size = 1) +
  annotate(geom = 'text', label = 'Social cost of carbon\n($46/ton)', x = 1.5, y = 60, color = 'white', size = 3) +
  scale_y_continuous(name = parse(text = 'Cost~per~ton~CO[2]~reduced'), breaks = c(-2000,-1000,0,1000), labels = c('-$2000','-$1000','$0','$1000')) +
  scale_x_discrete(labels = c('Cost of implementation', 'Net cost to society')) +
  theme_black() +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(color = c('green','green','white','red')),
        panel.grid = element_blank())

ggsave('/nfs/qread-data/figures/secondyeartalk/netcost.png', height = 5, width = 5, dpi = 300)


# Export cost table
cost_table %>% 
  rename(net_savings_per_ton_co2_avoided = net_cost_per_ton,
         cost_per_ton_co2_avoided = cost_per_ton,
         value_of_avoided_food_waste = avoided_value,
         price_used_for_avoided_food = class,
         net_savings = net_cost) %>%
  arrange(cost_per_ton_co2_avoided) %>%
  write_csv('/nfs/qread-data/csv_exports/quick_and_dirty_cost_per_ton_co2.csv')
