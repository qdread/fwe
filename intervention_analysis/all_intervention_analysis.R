# All-intervention analysis (preliminary "messy" analysis before putting into clean Rmd document)
# QDR / FWE / 24 Feb 2020


# Standardized date labeling ----------------------------------------------

# Edit 27 March 2020: Get rid of 0% coordination scenario, since it is not realistic. Update costs and convert all costs to 2012 dollars.

# One-time costs for two scenarios: no coordination and coordination (remove coordination )
# Get them from the document

library(tidyverse)
library(readxl)
library(zoo)
library(reticulate)

trunc_ellipsis <- function(x, n) if_else(nchar(x) < n, x, paste(substr(x, 1, n), '...')) # Convenience function to cut off long labels.

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub', '~'))

# Load the BEA code data (erroneously called NAICS) to get the codes
bea_codes <- read_csv(file.path(fp, 'crossreference_tables/naics_crosswalk_final.csv'))

# Demand codes table to convert 6 digit codes to the ones used by USEEIO
all_codes <- read_csv(file.path(fp, 'crossreference_tables/all_codes.csv'))

datelabel_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/intervention_costs_26mar2020/costs for date labeling change_3-26-2020.xlsx'), skip = 9)

#datelabel_costs_nocoord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (w/o coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))
datelabel_costs_coord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (with coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))

# We have one-time costs but need to annualize using 5 years at 7% interest as was done for waste tracking.

# Annuity function as implemented in excel, f and t are zero.
pmt <- function(p, r, n, f, t) (p * r * (1+r)^n  - f) / (((1+r)^n - 1) * (1 + r * t))

#(datelabel_costs_nocoord_annual <- pmt(datelabel_costs_nocoord, r = 0.07, n = 5, f = 0, t = 0)) # 350m to 1.4b
(datelabel_costs_coord_annual <- pmt(datelabel_costs_coord, r = 0.07, n = 5, f = 0, t = 0)) # 35m to 283m. This matches Mary's calculations.

# Update 27 Mar 2020: We do not need to do an offset for materials because we're assuming 100% coordination and no materials are used.

#### 
# Waste reduction rates from date labeling standardization, and which categories it acts on.
consumer_response <- c(lower = 0.05, upper = 0.10, mean = 0.075)
# Refed also assumes 20% of avoidable household waste is due to confusion over expiration dates (this seems high so set it as an upper bound)
proportion_confusion_waste <- c(lower = 0.10, upper = 0.20, mean = 0.15)

# If baseline avoidable household rate is ~ 20 to 25 percent, then the baseline confusion rate is 0.20 * 0.20 = 0.04. If 5 to 10 percent of consumers 
# change their behavior, the post-intervention confusion rate is 3.6 to 3.8%.

# So we can use the baseline confusion rate for the baseline waste demand, then the post-intervention rate, for all households.
# 0.002 to 0.004 of all final consumer food demand, roughly. That actually seems pretty plausible.

# Using all final demand from levels 1 and 3, multiplied by the appropriately adjusted pre-intervention and post-intervention "confusion waste rate".
finaldemand2012 <- read_csv(file.path(fp_github, 'USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'))

# Load waste rates for the BEA codes, calculated in lafa_rate_conversion.R
bea_waste_rates <- read_csv(file.path(fp, 'crossreference_tables/waste_rates_bea.csv'))

# Consumer demand baseline, averted in lower bound scenario, and averted in upper bound scenario
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# baseline waste rate is the baseline avoidable consumer waste rate times the proportion of that waste that is "confusion"
# waste reduction rate is the consumer response
# proportion food is the % food in that BEA code

# We also need to include a very simple accounting of beverages.
# 311920: coffee and tea, 311930 drink concentrates, 312110 soft drinks, bottled water, and ice.
# This is supplemented with very crude numbers I estimated from lit sources, which appear in the stoten II paper.
beverage_waste_rates <- bea_codes %>% 
  filter(beverages > 0, stage == 'processing') %>% 
  filter(!grepl('beer|wine|spirits', BEA_389_def)) %>%
  select(BEA_389_code, BEA_389_def) %>%
  mutate(primary_loss_value = 4.5, retail_loss_value = 5, avoidable_consumer_loss_value = 8)

bea_waste_rates <- bind_rows(bea_waste_rates, beverage_waste_rates)

datelabelingdemand <- finaldemand2012 %>%
  right_join(bea_waste_rates) %>%
  left_join(bea_codes) %>%
  mutate(baseline_demand = `2012_US_Consumption` * proportion_food,
         baseline_consumer_waste_demand = baseline_demand * avoidable_consumer_loss_value / 100,
         averted_demand_mean = `2012_US_Consumption` * (1 - demand_change_fn(W0 = proportion_confusion_waste['mean'] * avoidable_consumer_loss_value / 100,
                                                                              r = consumer_response['mean'],
                                                                              p = proportion_food)),
         averted_demand_lower = `2012_US_Consumption` * (1 - demand_change_fn(W0 = proportion_confusion_waste['lower'] * avoidable_consumer_loss_value / 100,
                                                                        r = consumer_response['lower'],
                                                                        p = proportion_food)),
         averted_demand_upper = `2012_US_Consumption` * (1 - demand_change_fn(W0 = proportion_confusion_waste['upper'] * avoidable_consumer_loss_value / 100,
                                                                        r = consumer_response['upper'],
                                                                        p = proportion_food))) %>%
  select(BEA_389_code, BEA_389_def, baseline_demand, baseline_consumer_waste_demand, averted_demand_mean, averted_demand_lower, averted_demand_upper)


# Join with long code names
datelabelingdemand <- datelabelingdemand %>%
  left_join(all_codes[,c(1,3)], by = c('BEA_389_code' = 'sector_code_uppercase'))

# Run EEIO for the baseline, averted mean, averted lower, and averted upper values
# For now, just sum everything up across food types (not that important which is which)

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

datelabeling_baseline_eeio <- with(datelabelingdemand, eeio_lcia('USEEIO2012', as.list(baseline_demand), as.list(sector_desc_drc)))
datelabeling_avertedmean_eeio <- with(datelabelingdemand, eeio_lcia('USEEIO2012', as.list(averted_demand_mean), as.list(sector_desc_drc)))
datelabeling_avertedlower_eeio <- with(datelabelingdemand, eeio_lcia('USEEIO2012', as.list(averted_demand_lower), as.list(sector_desc_drc)))
datelabeling_avertedupper_eeio <- with(datelabelingdemand, eeio_lcia('USEEIO2012', as.list(averted_demand_upper), as.list(sector_desc_drc)))

# Convert EEIO output into a single data frame

eeio_datelabeling <- map2_dfr(list(datelabeling_baseline_eeio, datelabeling_avertedmean_eeio, datelabeling_avertedlower_eeio, datelabeling_avertedupper_eeio),
                              c('impact_baseline', 'impact_averted_mean', 'impact_averted_lower', 'impact_averted_upper'),
                              ~ data.frame(category = row.names(.x),
                                           scenario = .y,
                                           impact = .x[,'Total']))


# Combine impact and offset to get net impact reduced
eeio_datelabeling_result <- eeio_datelabeling %>% 
  pivot_wider(names_from = scenario, values_from = impact) %>%
  #left_join(datelabeling_offset_eeio) %>%
  mutate(net_averted_mean_coordination = impact_averted_mean,
         net_averted_lower_coordination = impact_averted_lower,
         net_averted_upper_coordination = impact_averted_upper,
         net_percent_averted_mean_coordination = 100 * net_averted_mean_coordination / impact_baseline,
         net_percent_averted_lower_coordination = 100 * net_averted_lower_coordination / impact_baseline,
         net_percent_averted_upper_coordination = 100 * net_averted_upper_coordination / impact_baseline)
# It averts 0.1% to 0.4% of the food system's environmental impact. That's plausible, if anything high.
  
# Cost per unit reduction for date labeling, using annualization of one-time costs
eeio_datelabeling_result <- eeio_datelabeling_result %>%
  mutate(cost_per_reduction_mean_coordination = datelabel_costs_coord_annual['mean'] / net_averted_mean_coordination,
         cost_per_reduction_lower_coordination = datelabel_costs_coord_annual['lower'] / net_averted_upper_coordination,
         cost_per_reduction_upper_coordination = datelabel_costs_coord_annual['upper'] / net_averted_lower_coordination)

# Display the results.

conversion_factors <- c(1e-9, 1e-6, 1e-9, 1e-10, 1e-9)
category_names <- c('energy (PJ)', 'eutrophication (kT N)', 'greenhouse gas (MT CO2)', 'land (Mha)', 'water (km3)')

datelabeling_impact_data <- eeio_datelabeling_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('net')) %>%
  mutate(category = category_names) %>%
  mutate_at(vars(contains('net_averted')), ~ .* conversion_factors)

datelabeling_cost_data <- eeio_datelabeling_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('cost'))
# 
# datelabeling_cost_data %>%
#   mutate(category = c('energy ($/MJ)', 'eutrophication ($/kg N)', 'greenhouse gas ($/kg CO2)',  'land ($/m2)', 'water ($/m3)')) %>%
#   filter(!grepl('eutr', category)) %>%
#   pivot_longer(-category) %>%
#   mutate(bound = case_when(grepl('lower', name) ~ 'lower',
#                            grepl('upper', name) ~ 'upper',
#                            TRUE ~ 'mean'),
#          coordination = if_else(grepl('nocoordination', name), 'no', 'yes')) %>%
#   select(-name) %>%
#   pivot_wider(names_from = bound, values_from = value) %>%
#   ggplot(aes(x = coordination, color = coordination, y = mean, ymin = lower, ymax = upper)) +
#     geom_point(size = 2) +
#     geom_errorbar(size = 1, width = 0.1) +
#     facet_wrap(~ category, scales = 'free_y') +
#     scale_y_continuous(labels = scales::dollar) +
#     theme_bw() + 
#     theme(panel.grid = element_blank(), strip.background = element_blank(), legend.position = 'none')
# 
# ggsave(file.path(fp, 'figures/intervention_analysis/date_labeling_cost_per_reduction.pdf'), height = 7, width = 8)

# Save result
write_csv(eeio_datelabeling_result, file.path(fp, 'scenario_results/interventions/eeio_datelabeling_all.csv'))

datelabeling_impact_data_tocsv <- eeio_datelabeling_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('net')) %>%
  mutate_at(vars(contains('net_averted')), ~ .* conversion_factors)

datelabeling_cost_data_tocsv <- eeio_datelabeling_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, starts_with('cost'))

datelabeling_cost_data_tocsv %>%
  left_join(datelabeling_impact_data_tocsv) %>%
  write_csv(file.path(fp, 'scenario_results/interventions/eeio_datelabeling_5categories_processed.csv'))

# Spoilage prevention packaging -------------------------------------------

# Edit 27 March 2020: Update the costs with 2012 dollars. The new costs apply to fresh fruit, vegetables, meat, and poultry, but not seafood.
# Assume same % of fruit and vegetables are packaged. 1/3 packaging for both fruit and veg, and 1/2 for meat and poultry.

# For packaging, we are going to use one-time costs, then per-unit costs. 
# Mary has already done the cost totals. 
# The annualized initial cost is relatively small; most of it is the annual cost. We will represent 100% of the annual cost being materials.

# We now use ReFED's assumptions that this can be done for 15% of fruit and 25% of meat, with 10-33% retail waste reduction and 5-10% residential waste reduction.

packaging_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/intervention_costs_26mar2020/costs for intelligent packaging_3-26-2020.xlsx'), skip = 10, col_names = c('cost_type_2012_dollars', 'low', 'mean', 'high', 'notes')) 

packaging_annual_equipment_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Total annual costs', 2:4]
packaging_total_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Total annualized and annual costs', 2:4]
packaging_initial_costs <- packaging_costs[packaging_costs$cost_type_2012_dollars %in% 'Annualized initial costs (5 years, 7%)', 2:4]

# We also use the values from the cost model for material costs to get the one-time materials costs, and annualize them too.
# Cost of materials for packaging assessment are lower $86, median $100, upper $114, per formula.
# This is fairly negligible since there are only 18416 formulas so the cost is around 1.8 million, but it can be accounted for.

# There is retail and household loss reduction. Load table of rates.
packaging_reduction_rates <- read_csv(file.path(fp, 'scenario_inputdata/reduction_rate_packaging.csv'))

# Load use table to find the purchases of fruit and meat by retailers.

# BEA levels 1+3 to 4+6+7+8 is already subsetted from an older analysis I did.
food_U <- read.csv(file.path(fp, 'crossreference_tables/level13_to_level4678_inputs.csv'), row.names = 1, check.names = FALSE)

# rows will be fruit and meat rows, columns will be retail.

# Load the BEA code data (erroneously called NAICS) to get the codes
bea_codes <- read_csv(file.path(fp, 'crossreference_tables/naics_crosswalk_final.csv'))

retail_codes <- bea_codes$BEA_389_code[bea_codes$stage %in% 'retail']

# Get the rows for fruit and meat
fruit_rows <- grep("fruit", bea_codes$BEA_389_def, ignore.case = TRUE)
# The fresh fruit code is 100% fruit, and the processed fruit code is 74.2% fruit and vegetables, but not all of that is fruit. Need to go back and look.

U <- read.csv(file.path(fp, 'raw_data/BEA/formatted/use2012.csv'), stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)

prop_fruit_veg <- U[c('111200', '111300'), c('311420')]
prop_processed_fruit <- prop_fruit_veg[2]/sum(prop_fruit_veg) # 85.16% by $ value of the processed fruit/veg industry is fruit. (This isn't needed anymore)

food_U[fruit_rows, retail_codes] # This is basically zero. We will need to just change the final consumer demand by the product of the waste rates, and use consumer price.

##########
# LAFA rate conversion for the fruit and meat codes in LAFA.
# These are probably more than needed.
fruit_meat <- bea_codes %>% filter(stage %in% c('agriculture','processing'),
                                  fruit_veg_fresh > 0 | fruit_veg_processed > 0 | meat > 0 | fish_fresh > 0)

# We need baseline retail loss and consumer loss for fruits and meats

# Load LAFA
source(file.path(fp_github, 'fwe/read_data/read_lafa.r'))
lafa <- list(veg, fruit, meat)

# Read the description of LAFA's nested category structure in.
lafa_struct <- read_csv(file.path(fp, 'crossreference_tables/lafa_category_structure.csv'))

# find the single year closest to 2012.
lafa_df <- lafa %>%
  map2_dfr(c('veg', 'fruit','meat'), ~ select(.x, Category, Year, Retail_weight_Lbs.year, Loss_from_retail__institutional_to_consumer_level_Percent, Consumer_weight_Lbs.year,
                   Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent) %>%
             mutate(Group = .y)) %>%
  setNames(c('Food','Year','retail_weight','retail_loss','consumer_weight','avoidable_consumer_loss','Group')) %>%
  filter(!is.na(avoidable_consumer_loss)) %>%
  group_by(Group, Food) %>%
  filter(abs(Year - 2012) == min(abs(Year - 2012)))

lafa_df <- lafa_df %>% 
  ungroup %>%
  left_join(lafa_struct) %>%
  filter(subgroup2 == 'Fresh fruit' | subgroup1 %in% c('Fresh vegetables', 'Red meat', 'Poultry')) %>%
  mutate(group_final = if_else(Group == 'fruit', subgroup2, subgroup1)) %>%
  select(group_final, Food:avoidable_consumer_loss)

fruitmeat_wtdavg_rates <- lafa_df %>%
  group_by(group_final) %>%
  summarize(retail_loss = weighted.mean(retail_loss, retail_weight),
            avoidable_consumer_loss = weighted.mean(avoidable_consumer_loss, consumer_weight))

# We now have the baseline retail and consumer waste rate for 4 groups: fresh fruit, poultry, seafood, and red meat.

### Load the baseline consumer demand for fresh fruit, red meat, poultry, and seafood in 2012.
# Do not include processed and frozen products, only fresh.
#fruit_meat_codes <- c('111300', '311615', '31161A', '112A00', '114000') # fresh fruit, pkg poultry, pkg meat, aquaculture, wild caught fish
#fruit_meat_proportions <- c(1, 1, 1, 0.113, 0.757) # the final two categories that contain seafood are not all seafood. (source: QCEW)
# Update: Now we are using vegetables but not seafood.
fruit_meat_codes <- c('111200', '111300', '311615', '31161A')
fruit_meat_proportions <- rep(1, 4)

finaldemand2012 <- read_csv(file.path(fp_github, 'USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'))

fruitmeatdemand2012 <- data.frame(BEA_389_code = fruit_meat_codes,
                                  proportion = fruit_meat_proportions) %>%
  left_join(finaldemand2012)

# We need to deduct the non-fish proportion of the seafood demand categories.

# Post intervention demand under different scenarios
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

packaging_reduction_rates_wide <- packaging_reduction_rates %>%
  pivot_wider(id_cols = food, names_from = level, values_from = -c(food,level))

# Compute baseline demand (deducting non-fish proportion of the seafood codes)
# and reduction rates of retail and household demand (lower and upper bounds)
# from this get the averted demand after the intervention for each food
fruitmeatdemand2012 <- fruitmeatdemand2012 %>%
  mutate(food = c('fruit','fruit','meat','meat'), group_final = c('Fresh vegetables', 'Fresh fruit', 'Poultry', 'Red meat')) %>%
  left_join(fruitmeat_wtdavg_rates) %>%
  left_join(packaging_reduction_rates_wide) %>%
  mutate(baseline_demand = `2012_US_Consumption` * proportion,
         retail_reduction_lower = demand_change_fn(W0 = retail_loss/100, r = waste_reduction_lower_retail, p = proportion_affected_retail),
         retail_reduction_upper = demand_change_fn(W0 = retail_loss/100, r = waste_reduction_upper_retail, p = proportion_affected_retail),
         household_reduction_lower = demand_change_fn(W0 = avoidable_consumer_loss/100, r = waste_reduction_lower_household, p = proportion_affected_household),
         household_reduction_upper = demand_change_fn(W0 = avoidable_consumer_loss/100, r = waste_reduction_upper_household, p = proportion_affected_household),
         demand_reduction_lower = retail_reduction_lower * household_reduction_lower,
         demand_reduction_upper = retail_reduction_upper * household_reduction_upper,
         demand_averted_lower = baseline_demand * (1 - demand_reduction_lower),
         demand_averted_upper = baseline_demand * (1 - demand_reduction_upper))

c(sum(fruitmeatdemand2012$demand_averted_lower), sum(fruitmeatdemand2012$demand_averted_upper))/1e9 # 1.34 to 3.12 billion dollars.

# Get full codes that will be recognized by USEEIO.
fruitmeatdemand2012 <- all_codes %>%
  select(sector_desc_drc, sector_code_uppercase) %>%
  right_join(fruitmeatdemand2012, by = c('sector_code_uppercase' = 'BEA_389_code'))

### 
# Run EEIO for the baseline, demand averted lower, and demand averted upper
# Do separately for each food so that we can see the result for each one.

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

# 4 categories x (1 baseline+1averted lower+1averted upper) = 12 runs of model
# Already in units of dollars so don't need to multiply by any factor.
eeio_packaging_averted <- pmap(fruitmeatdemand2012, function(sector_desc_drc, baseline_demand, demand_averted_lower, demand_averted_upper, ...) {
  demand_code <- as.list(sector_desc_drc)
  list(baseline = eeio_lcia('USEEIO2012', as.list(baseline_demand), demand_code),
       averted_lower = eeio_lcia('USEEIO2012', as.list(demand_averted_lower), demand_code),
       averted_upper = eeio_lcia('USEEIO2012', as.list(demand_averted_upper), demand_code))
})

# Put this into a data frame
eeio_packaging_averted <- map2_dfr(c('fresh vegetables', 'fresh fruit', 'poultry', 'meat'), eeio_packaging_averted, function(x, y) {
  impacts <- do.call(cbind, y) %>% setNames(names(y))
  data.frame(food = x, category = row.names(impacts), impacts)
})

conversion_factors <- c(1e-9, 1e-6, 1e-9, 1e-10, 1e-9)
category_names <- c('energy (PJ)', 'eutrophication (kT N)', 'greenhouse gas (MT CO2)', 'land (Mha)', 'water (km3)')

# Make a preliminary plot to show how much of the impact is averted
errorbarplot_data <- eeio_packaging_averted %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate_if(is.numeric, ~ . * conversion_factors) %>%
  mutate(category = rep(category_names, times = nrow(.)/5),
         food = factor(food, levels = c('meat', 'poultry', 'fresh vegetables', 'fresh fruit'))) 


pkg_errorbarplot <- ggplot(errorbarplot_data, aes(x = food, ymin = averted_lower, ymax = averted_upper)) +
  geom_errorbar(size = 1, width = 0.15) +
  facet_wrap(~ category, scales = 'free') + 
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_blank()) +
  ggtitle('Environmental impacts averted by spoilage-prevention packaging',
          'uncertainty range due to ReFED\'s estimate of waste reduction rate')

ggsave(file.path(fp, 'figures/intervention_analysis/packaging_impact_averted.pdf'), pkg_errorbarplot, height = 6, width = 9)


##### offsetting impacts for the possible industry codes for packaging materials and equipment
# for now, do this as per $1 spent on each category so it can be multiplied by the different costs.

intervention_industry_BEA <- read_csv(file.path(fp, 'scenario_inputdata/intervention_industry_BEA.csv'))

# Get packaging industries and join with full length codes
packaging_industries <- intervention_industry_BEA %>%
  filter(industry_type %in% 'packaging materials' | BEA_Title %in% 'Packaging machinery manufacturing') %>%
  left_join(all_codes %>% select(sector_desc_drc, sector_code_uppercase), by = c('BEA_Code' = 'sector_code_uppercase'))

eeio_packaging_offsetting_impacts <- packaging_industries %>%
  group_by(BEA_Code, BEA_Title, industry_type) %>%
  group_modify(function(x, ...) {
    eeio <- eeio_lcia('USEEIO2012', list(1), list(x$sector_desc_drc))
    data.frame(category = row.names(eeio), impact = eeio[,'Total'])
  })

packaging_offset_perdollar_plot <- eeio_packaging_offsetting_impacts %>%
  ungroup %>%
  filter(grepl('enrg|gcc|land', category)) %>%
  mutate(category = rep(c('Energy (MJ)', 'GHG (kg CO2 eq.)', 'Land (m2)'), times = nrow(.)/3),
         BEA_Title = trunc_ellipsis(BEA_Title, 50)) %>%
  mutate(BEA_Title = factor(BEA_Title, levels = unique(BEA_Title))) %>%
  ggplot(aes(x = BEA_Title, y = impact)) +
    facet_grid(. ~ category, scales = 'free_x') +
    geom_col() +
    coord_flip() +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
    theme_bw() +
    theme(panel.grid = element_blank(), strip.background = element_blank()) +
    ggtitle('Packaging environmental impact per $1 final demand',
            'for packaging machinery and many possible types of packaging materials')

ggsave(file.path(fp, 'figures/intervention_analysis/packaging_impact_offset_perdollar.pdf'), packaging_offset_perdollar_plot, height = 8.5, width = 11)

#### cost estimates
# use the annualized initial cost to get the initial impacts, using packaging machinery as the industry
# use the annual costs to get the annual impacts, using plastic packaging materials as the industry.

packaging_annual_offset <- eeio_packaging_offsetting_impacts %>%
  filter(grepl('Plastics packaging', BEA_Title)) %>%
  mutate(offset_lower = impact * packaging_annual_equipment_costs$low,
         offset_mean = impact * packaging_annual_equipment_costs$mean,
         offset_upper = impact * packaging_annual_equipment_costs$high)

# for the annualized initial cost, we use $86, $100, $114 per formula times the number of formulas.
n_formulas <- 18416
materials_cost_per_formula <- c(low = 86, mean = 100, high = 114)
pmt(p = n_formulas * materials_cost_per_formula, r = 0.07, n = 5, t = 0, f = 0)
# This is basically negligible so I will ignore it.

offsetplot_data <- packaging_annual_offset %>%
  ungroup %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate_if(is.numeric, ~ . * conversion_factors) %>%
  mutate(category = rep(category_names, times = nrow(.)/5))

impactandoffset_plot_data <- errorbarplot_data %>%
  group_by(category) %>%
  summarize_if(is.numeric, sum) %>%
  left_join(offsetplot_data %>% select(category, contains('offset'))) %>%
  mutate(net_lower = averted_lower - offset_upper,
         net_upper = averted_upper - offset_lower)

impactandoffset_plot_data_long <- impactandoffset_plot_data %>%
  select(-baseline) %>%
  pivot_longer(-category) %>%
  separate(name, into = c('impact', 'bound')) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(impact = factor(impact, levels = c('averted', 'offset', 'net')))

impactandoffset_plot <- ggplot(impactandoffset_plot_data_long, aes(x = impact, ymin = lower, ymax = upper, color = impact)) +
  facet_wrap(~ category, scales = 'free_y') +
  geom_errorbar(size = 1, width = 0.1) +
  scale_y_continuous(name = "impact", expand = expand_scale(mult = c(0, 0.1))) +
  expand_limits(y = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_blank(), axis.title.x = element_blank(), legend.position = 'none') +
  ggtitle("Net environmental impact averted by packaging intervention",
          "offset by impacts of producing additional plastic packaging material")

ggsave(file.path(fp, 'figures/intervention_analysis/packaging_net_impact_averted_total.pdf'), impactandoffset_plot, height = 6, width = 9)

#### cost per unit impact reduction.
# Total annualized and annual costs divided by total net impact averted


cost_per_reduction_packaging <- eeio_packaging_averted %>%
  group_by(category) %>%
  summarize_if(is.numeric, sum) %>%
  left_join(packaging_annual_offset %>% ungroup %>% select(category, contains('offset'))) %>%
  mutate(net_lower = averted_upper - offset_lower,
         net_upper = averted_lower - offset_upper) %>%
  group_by(category) %>%
  group_modify(~ as.data.frame(outer(as.numeric(packaging_total_costs), as.numeric(.[,-(1)]), `/`))) %>%
  ungroup %>%
  setNames(c("category",      "averted_lower", "averted_upper", "offset_lower",  "offset_mean",   "offset_upper", "net_lower", "net_upper")) %>%
  mutate(bound = rep(c('lower','mean','upper'), times = nrow(.)/3)) %>%
  select(category, bound, net_lower, net_upper) %>%
  group_by(category) %>%
  summarize(net_lower = min(net_lower), net_upper = max(net_upper))

cost_per_reduction_packaging %>% 
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate(category = c('eutrophication ($/kg N)', 'greenhouse gas ($/kg CO2)', 'energy ($/MJ)', 'land ($/m2)', 'water ($/m3)'))
# Net result: 5 to 36 cents per kg CO2 averted. Compares favorably to WTA. Scale could be interesting too.
         
# Save results

write_csv(eeio_packaging_averted, file.path(fp, 'scenario_results/interventions/eeio_packaging_byfoodtype_all.csv'))
write_csv(errorbarplot_data, file.path(fp, 'scenario_results/interventions/eeio_packaging_byfoodtype_5categories_processed.csv'))
write_csv(impactandoffset_plot_data, file.path(fp, 'scenario_results/interventions/eeio_packaging_byfoodtype_5categories_withoffset.csv'))
write_csv(cost_per_reduction_packaging, file.path(fp, 'scenario_results/interventions/eeio_packaging_costperreduction_all.csv'))

# Put results from packaging into a "standardized" form that is similar to the other ones.

eeio_packaging_averted_total <- eeio_packaging_averted %>%
  group_by(category) %>%
  summarize_if(is.numeric, sum)

# This includes some irrelevant categories of packaging material
# eeio_packaging_offsetting_impacts

eeio_packaging_result <- eeio_packaging_averted_total %>%
  left_join(packaging_annual_offset %>% ungroup %>% select(category, contains('offset'))) %>%
  mutate(net_averted_lower = averted_lower - offset_upper,
         net_averted_mean = (averted_lower + averted_upper) / 2 - offset_mean,
         net_averted_upper = averted_upper - offset_lower)

# Add costs
eeio_packaging_result <- eeio_packaging_result %>%
  cbind(packaging_total_costs %>% setNames(paste0('total_cost_', names(.)))) %>%
  mutate(cost_per_reduction_lower = total_cost_low / net_averted_upper,
         cost_per_reduction_mean = total_cost_mean / net_averted_mean,
         cost_per_reduction_upper = total_cost_high / net_averted_lower)

write_csv(eeio_packaging_result, file.path(fp, 'scenario_results/interventions/eeio_packaging_all.csv'))


# Consumer education campaigns --------------------------------------------

# Edited 26 March 2020: convert costs to 2012 dollars, increase frequency of low and high number of campaigns per year to 6 and 12

# Assumptions: 

# Reduction in rate of food waste
consumer_ed_waste_reduction <- (1/3) * (2/3) * c(.05,.1,.15) # 1.1 to 3.3 percent reduction, we can use this fairly small number

# Proportion of baseline demand affected is equal to the proportion of population who lives in the metropolitan areas that are big enough to get a targeted campaign

# Cost is the annual cost per campaign multiplied by the number of metropolitan areas that will have campaigns

# The offsetting impact should be fairly negligible, using the environmental impact from media industries which will be very low per $1 output.

# There are 1251 counties within metropolitan statistical areas, together representing 85.6% of the US pop
pop_in_metro <- 0.856 # see read_msas.R for derivation of this number.
n_metro_counties <- 1251

# Costs: content development 1x per year regardless, media consultant 6-12x per year (low/high), media costs 6-12x per year (low/high)
consumer_ed_costs <- c(content_development = 68.6e3,
                       media_consultant = 15.68e3,
                       media_costs = 29.4e3)

consumer_ed_costs_annual <- n_metro_counties * c(lower = sum(consumer_ed_costs * c(1,6,6)),
                                                 upper = sum(consumer_ed_costs * c(1,12,12)))
# 424 to 763 million

# Calculate baseline demand & baseline waste at consumer level, multiplied by the 85.6% number.

# Using all final demand from levels 1 and 3, multiplied by the appropriately adjusted pre-intervention and post-intervention "confusion waste rate".
finaldemand2012 <- read_csv(file.path(fp_github, 'USEEIO/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'))

# Load waste rates for the BEA codes, calculated in lafa_rate_conversion.R
bea_waste_rates <- read_csv(file.path(fp, 'crossreference_tables/waste_rates_bea.csv'))

# Consumer demand baseline, averted in lower bound scenario, and averted in upper bound scenario
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# add beverage rates (copied from above)
beverage_waste_rates <- bea_codes %>% 
  filter(beverages > 0, stage == 'processing') %>% 
  filter(!grepl('beer|wine|spirits', BEA_389_def)) %>%
  select(BEA_389_code, BEA_389_def) %>%
  mutate(primary_loss_value = 4.5, retail_loss_value = 5, avoidable_consumer_loss_value = 8)

bea_waste_rates <- bind_rows(bea_waste_rates, beverage_waste_rates)

############ edit the following to have lower, mean, and upper, and make sure numbers are correct
consumer_ed_demand <- finaldemand2012 %>%
  right_join(bea_waste_rates) %>%
  left_join(bea_codes) %>%
  mutate(baseline_demand = `2012_US_Consumption` * proportion_food,
         baseline_demand_metro = baseline_demand * pop_in_metro,
         baseline_consumer_waste_demand = baseline_demand_metro * avoidable_consumer_loss_value / 100,
         averted_demand_lower = `2012_US_Consumption` * (1 - demand_change_fn(W0 = avoidable_consumer_loss_value / 100,
                                                                              r = consumer_ed_waste_reduction[1],
                                                                              p = proportion_food * pop_in_metro)),
         averted_demand_mean = `2012_US_Consumption` * (1 - demand_change_fn(W0 = avoidable_consumer_loss_value / 100,
                                                                              r = consumer_ed_waste_reduction[2],
                                                                              p = proportion_food * pop_in_metro)),
         averted_demand_upper = `2012_US_Consumption` * (1 - demand_change_fn(W0 = avoidable_consumer_loss_value / 100,
                                                                              r = consumer_ed_waste_reduction[3],
                                                                              p = proportion_food * pop_in_metro))) %>%
  select(BEA_389_code, BEA_389_def, baseline_demand, baseline_demand_metro, baseline_consumer_waste_demand, averted_demand_lower, averted_demand_mean, averted_demand_upper)


# Join with long code names
consumer_ed_demand <- consumer_ed_demand %>%
  left_join(all_codes[,c(1,3)], by = c('BEA_389_code' = 'sector_code_uppercase'))

# Run EEIO for the baseline, averted lower, averted mean, and averted upper values
# For now, just sum everything up across food types (not that important which is which)

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

consumer_ed_baseline_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(baseline_demand), as.list(sector_desc_drc)))
consumer_ed_avertedlower_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(averted_demand_lower), as.list(sector_desc_drc)))
consumer_ed_avertedmean_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(averted_demand_mean), as.list(sector_desc_drc)))
consumer_ed_avertedupper_eeio <- with(consumer_ed_demand, eeio_lcia('USEEIO2012', as.list(averted_demand_upper), as.list(sector_desc_drc)))

# Convert EEIO output into a single data frame

eeio_consumer_ed <- map2_dfr(list(consumer_ed_baseline_eeio, consumer_ed_avertedlower_eeio, consumer_ed_avertedmean_eeio, consumer_ed_avertedupper_eeio),
                              c('impact_baseline', 'impact_averted_lower', 'impact_averted_mean', 'impact_averted_upper'),
                              ~ data.frame(category = row.names(.x),
                                           scenario = .y,
                                           impact = .x[,'Total']))

# Offsetting impacts
# Offsetting impacts, using the low and high rates of yearly media costs
# For content development, use specialized design services 541400
# For media consulting, use advertising/PR services 541800
# For media, use a combination of advertising, newspaper, magazine, radio, and internet publishing (use highest and lowest bound from these)

media_codes <- c('541400', '541800', '511110', '511120', '515100', '519130')

media_codes_long <- all_codes$sector_desc_drc[match(media_codes, all_codes$sector_code_uppercase)]

consumer_ed_offset_eeio <- map(media_codes_long, ~ eeio_lcia('USEEIO2012', list(1), list(.))) # offset per dollar on each media industry

consumer_ed_offset_eeio <- map2_dfr(consumer_ed_offset_eeio, media_codes_long, ~ data.frame(BEA_code = .y,
                                                                                            category = row.names(.x),
                                                                                            impact = .x[,'Total']))

consumer_ed_offset_eeio %>% filter(grepl('gcc',category)) # They vary by a factor of 2 at most.

# Calculate upper and lower bounds for offset based on assigning media impacts to upper and lower most impactful industries

# Calculate upper and lower bounds based on costs
consumer_ed_costs_lower <- n_metro_counties * consumer_ed_costs * c(1,6,6)
consumer_ed_costs_upper <- n_metro_counties * consumer_ed_costs * c(1,12,12)

# Find lower and upper limits for the media component
media_impacts <- consumer_ed_offset_eeio %>%
  filter(!BEA_code %in% media_codes_long[1]) %>%
  group_by(category) %>%
  summarize(media_costs_impact_lower = min(impact), media_costs_impact_upper = max(impact), media_costs_impact_median = median(impact))

consumer_ed_impacts_bytype <- consumer_ed_offset_eeio %>%
  filter(BEA_code %in% media_codes_long[1:2]) %>%
  pivot_wider(names_from = BEA_code, values_from = impact) %>%
  setNames(c('category', 'content_development', 'media_consultant')) %>%
  left_join(media_impacts)

consumer_ed_costs_mean <- (consumer_ed_costs_lower + consumer_ed_costs_upper) / 2

consumer_ed_offset_df <- consumer_ed_impacts_bytype %>%
  mutate(offset_lower = rowSums(sweep(.[,c('content_development', 'media_consultant', 'media_costs_impact_lower')], 2, consumer_ed_costs_lower, `*`)),
         offset_median = rowSums(sweep(.[,c('content_development', 'media_consultant', 'media_costs_impact_median')], 2, consumer_ed_costs_mean, `*`)),
         offset_upper = rowSums(sweep(.[,c('content_development', 'media_consultant', 'media_costs_impact_upper')], 2, consumer_ed_costs_upper, `*`)))

# Combine impact and offset to get net impact reduced
eeio_consumer_ed_result <- eeio_consumer_ed %>% 
  pivot_wider(names_from = scenario, values_from = impact) %>%
  left_join(consumer_ed_offset_df) %>%
  mutate(net_averted_lower = impact_averted_lower - offset_upper,
         net_averted_mean = impact_averted_mean - offset_median,
         net_averted_upper = impact_averted_upper - offset_lower,

         net_percent_averted_lower = 100 * net_averted_lower / impact_baseline,
         net_percent_averted_median = 100 * net_averted_mean / impact_baseline,
         net_percent_averted_upper = 100 * net_averted_upper / impact_baseline)
# It averts 0.2 to 0.7% of the food system's environmental impact.


# Cost per unit reduction for date labeling, using annualization of one-time costs
# Turns out to be a fairly cheap method.
eeio_consumer_ed_result <- eeio_consumer_ed_result %>%
  mutate(cost_per_reduction_lower = sum(consumer_ed_costs_lower) / net_averted_upper,
         cost_per_reduction_mean = sum(consumer_ed_costs_mean) / net_averted_mean,
         cost_per_reduction_upper = sum(consumer_ed_costs_upper) / net_averted_lower)

# Display the results.

conversion_factors <- c(1e-9, 1e-6, 1e-9, 1e-10, 1e-9)
category_names <- c('energy (PJ)', 'eutrophication (kT N)', 'greenhouse gas (MT CO2)', 'land (Mha)', 'water (km3)')

consumer_ed_impact_data <- eeio_consumer_ed_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('net')) %>%
  mutate(category = category_names) %>%
  mutate_at(vars(contains('net_averted')), ~ .* conversion_factors)

consumer_ed_cost_data <- eeio_consumer_ed_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, starts_with('cost'))

consumer_ed_cost_data %>%
  mutate(category = c('energy ($/MJ)', 'eutrophication ($/kg N)', 'greenhouse gas ($/kg CO2)',  'land ($/m2)', 'water ($/m3)')) %>%
  filter(!grepl('eutr', category)) %>%
  ggplot(aes(x = category, color = category, y = cost_per_reduction_mean, ymin = cost_per_reduction_lower, ymax = cost_per_reduction_upper)) +
  geom_errorbar(size = 1, width = 0.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::dollar) +
  theme_bw() + 
  theme(panel.grid = element_blank(), legend.position = 'none')

ggsave(file.path(fp, 'figures/intervention_analysis/consumer_ed_cost_per_reduction.pdf'), height = 5, width = 5)

# Save result
write_csv(eeio_consumer_ed_result, file.path(fp, 'scenario_results/interventions/eeio_consumer_ed_all.csv'))

consumer_ed_impact_data_tocsv <- eeio_consumer_ed_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, contains('net')) %>%
  mutate_at(vars(contains('net_averted')), ~ .* conversion_factors)

consumer_ed_cost_data_tocsv <- eeio_consumer_ed_result %>%
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  select(category, starts_with('cost'))

consumer_ed_cost_data_tocsv %>%
  left_join(consumer_ed_impact_data_tocsv) %>%
  write_csv(file.path(fp, 'scenario_results/interventions/eeio_consumer_ed_5categories_processed.csv'))
