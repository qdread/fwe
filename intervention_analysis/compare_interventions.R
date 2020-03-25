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
library(directlabels)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_res <- file.path(fp, 'scenario_results/interventions')

result_consumered <- read_csv(file.path(fp_res, 'eeio_consumer_ed_all.csv'))
result_datelabeling <- read_csv(file.path(fp_res, 'eeio_datelabeling_all.csv'))
result_wta <- read_csv(file.path(fp_res, 'eeio_wta_bysector_all.csv'))
result_packaging_byfood <- read_csv(file.path(fp_res, 'eeio_packaging_byfoodtype_all.csv'))
result_packaging <- read_csv(file.path(fp_res, 'eeio_packaging_all.csv'))

# Total costs for WTA and packaging are in the DF.
# Total costs for consumer ed:
pop_in_metro <- 0.856 # see read_msas.R for derivation of this number.
n_metro_counties <- 1251

# Costs: content development 1x per year regardless, media consultant 1-2x per year (low/high), media costs 1-2x per year (low/high)
consumer_ed_costs <- c(content_development = 70e3,
                       media_consultant = 16e3,
                       media_costs = 30e3)

consumer_ed_costs_annual <- n_metro_counties * c(lower = sum(consumer_ed_costs),
                                                 mean = sum(consumer_ed_costs * c(1,1.5,1.5)),
                                                 upper = sum(consumer_ed_costs * c(1,2,2)))


# Total costs for date labeling:
datelabelcosts_coord_annual <- c(lower = 33282569.8605775, mean = 123208497.157611, upper = 286034468.241575
)
datelabelcosts_nocoord_annual <- c(lower = 349243816.792413, mean = 759583378.714786, upper = 1403967428.67742
)

# Combine into single DF --------------------------------------------------

# Data for total impacts and offsets (averted due to FLW reduction, offset, and net)

# Data for total cost

# get WTA cost
wta_cost <- result_wta %>% 
  group_by(group) %>% 
  slice(1) %>% 
  ungroup %>%
  select(total_cost_lower, total_cost_mean, total_cost_upper) %>%
  setNames(names(consumer_ed_costs_annual))

# get pkg cost
pkg_cost <- result_packaging %>%
  select(contains("total_cost")) %>%
  slice(1) %>%
  setNames(names(consumer_ed_costs_annual))

dat_totalcost <- data.frame(intervention = c('consumer education campaign', rep('standardized date labeling', 2), rep('waste tracking and analytics', 3), 'spoilage prevention packaging'),
                            scenario = c(NA, '0% coordinated', '100% coordinated', 'contracted foodservice', 'full-service restaurants', 'limited-service restaurants', NA),
                            rbind(consumer_ed_costs_annual,
                                  datelabelcosts_nocoord_annual,
                                  datelabelcosts_coord_annual,
                                  wta_cost,
                                  pkg_cost)
)

# Data for cost per unit impact reduction and for total impact averted (net)

# For WTA, split into a list by sector
result_wta_list <- result_wta %>%
  rename(impact_baseline = baseline) %>%
  mutate(offset_mean = (offset_lower + offset_upper) / 2) %>%
  select(group, category, impact_baseline, contains("offset"), contains("net_impact_averted"), contains("cost_per")) %>%
  setNames(gsub('_impact', '', names(.))) %>%
  mutate(intervention = 'waste tracking and analytics') %>%
  group_split(group) 

# For packaging, split into a list by food 
result_packaging_list <- result_packaging_byfood %>%
  group_split(food, keep = FALSE) %>%
  setNames(sort(unique(result_packaging_byfood$food)))


# Standardize the column names by intervention

all_interventions_list <- c(list(consumered = result_consumered %>%
       rename(offset_mean = offset_median) %>%
       select(category, impact_baseline, contains("offset"), contains("net_averted"), contains("cost_per")) %>%
         mutate(intervention = 'consumer education campaigns'),
     datelabeling_coordination = result_datelabeling %>%
       select(category, impact_baseline, contains("offset"), contains("_coordination")) %>%
       setNames(gsub('_coordination', '', names(.))) %>%
       mutate(intervention = 'standardized date labeling', group = '100% coordinated'),
     datelabeling_nocoordination = result_datelabeling %>%
       select(category, impact_baseline, contains("offset"), contains("_nocoordination")) %>%
       setNames(gsub('_nocoordination', '', names(.))) %>%
       mutate(intervention = 'standardized date labeling', group = '0% coordinated'),
     packaging = result_packaging %>%
       rename(impact_baseline = baseline) %>%
       select(category, impact_baseline, contains("offset"), contains("net_averted"), contains("cost_per")) %>%
       mutate(intervention = 'spoilage prevention packaging')
     ),
  result_wta_list)

#map(all_interventions_list, names)

all_interventions <- bind_rows(all_interventions_list) %>%
  select(intervention, group, everything())

# Total cost plot ---------------------------------------------------------

dat_totalcost$xpos = c(1, 2.8, 3.2, 4.8, 5, 5.2, 7)

ggplot(dat_totalcost, aes(y = mean/1e6, ymin = lower/1e6, ymax = upper/1e6, color = intervention, group = intervention, x = xpos)) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.1) +
  geom_text(aes(x = xpos - 0.5, label = gsub(' ', '\n', scenario)), color = 'black', alpha = 0.7) +
  scale_y_continuous(name = 'Total cost (million $)') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())


# Total impact averted plot -----------------------------------------------

all_interventions_4cat <- all_interventions %>%
  filter(grepl('enrg|gcc|land|watr', category)) %>%
  mutate_at(vars(contains('net_averted')), ~ . * c(1e-9, 1e-9, 1e-10, 1e-9)) %>%
  mutate(category = rep(c('energy (PJ)', 'greenhouse gas (MT CO2)', 'land (Mha)', 'water (km3)'), nrow(.)/4),
         category_cost = rep(c('energy (MJ)', 'greenhouse gas (kg CO2)', 'land (m2)', 'water (m3)'), nrow(.)/4)) 

ggplot(all_interventions_4cat, aes(x = intervention, group = group, color = intervention,
                                   y = net_averted_mean, ymin = net_averted_lower, ymax = net_averted_upper)) +
  facet_wrap(~ category, scales = 'free_y') +
  geom_point(size = 2) +
  geom_errorbar(width = 0.1) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

# Cost per impact averted plot --------------------------------------------

ggplot(all_interventions_4cat, aes(x = intervention, group = group, color = intervention,
                                   y = cost_per_reduction_mean, ymin = cost_per_reduction_lower, ymax = cost_per_reduction_upper)) +
  facet_wrap(~ category_cost, scales = 'free_y') +
  geom_point(size = 2) +
  geom_errorbar(width = 0.1) +
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())
