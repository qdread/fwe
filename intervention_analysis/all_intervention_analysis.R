# All-intervention analysis (preliminary "messy" analysis before putting into clean Rmd document)
# QDR / FWE / 24 Feb 2020


# Standardized date labeling ----------------------------------------------

# One-time costs for two scenarios: no coordination and coordination
# Get them from the document

library(tidyverse)
library(readxl)
library(zoo)
library(reticulate)

trunc_ellipsis <- function(x, n) if_else(nchar(x) < n, x, paste(substr(x, 1, n), '...')) # Convenience function to cut off long labels.

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub', '~'))

# Demand codes table to convert 6 digit codes to the ones used by USEEIO
all_codes <- read_csv(file.path(fp, 'crossreference_tables/all_codes.csv'))

datelabel_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/date_labeling/costs for date labeling change.xlsx'), skip = 9)

datelabel_costs_nocoord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (w/o coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))
datelabel_costs_coord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (with coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))

# We have one-time costs but need to annualize using 5 years at 7% interest as was done for waste tracking.

# Annuity function as implemented in excel, f and t are zero.
pmt <- function(p, r, n, f, t) (p * r * (1+r)^n  - f) / (((1+r)^n - 1) * (1 + r * t))

(datelabel_costs_nocoord_annual <- pmt(datelabel_costs_nocoord, r = 0.07, n = 5, f = 0, t = 0)) # 350m to 1.4b
(datelabel_costs_coord_annual <- pmt(datelabel_costs_coord, r = 0.07, n = 5, f = 0, t = 0)) # 33m to 286m

# Go back into the reformulation model output to see what portion of the total cost of the intervention is equipment, so we can get offsetting impact.
# For each model run, this will be in the tab "Detailed Cost" and each product category has a materials cost per UPC and total materials cost.
# The costs are split into labor and materials so we need to pull out the materials rows only.

### Do this with readxl

### 0% coordination
modelrun0 <- read_xlsx(file.path(fp, 'scenario_inputdata/date_labeling/date labeling model run-0% coordination.xlsx'), sheet = 'Detailed Costs', col_names = FALSE)

# carry over the first row to fill in the empty cells, then paste together rows 1 and 2 names to create the true header.
header_names <- gsub('NA_', '', paste(na.locf(unlist(modelrun0[1,]), na.rm = FALSE), unlist(modelrun0[2,]), sep = '_') )

modelrun0 <- modelrun0 %>%
  slice(-(1:2)) %>%
  setNames(header_names) %>%
  mutate_at(1:3, na.locf)

modelrun0_materials <- modelrun0 %>%
  filter(Cost_Type %in% 'Materials') %>%
  select(`Product Category`:`Brand Type`, Total_5th:Total_95th) %>%
  mutate_at(vars(Total_5th:Total_95th), as.numeric)

# Totals across all the food types
modelrun0_materials %>% select(Total_5th:Total_95th) %>% colSums # 330M to 789M

### 100% coordination
modelrun100 <- read_xlsx(file.path(fp, 'scenario_inputdata/date_labeling/date labeling model run-100% coordination.xlsx'), sheet = 'Detailed Costs', col_names = FALSE)

# carry over the first row to fill in the empty cells, then paste together rows 1 and 2 names to create the true header.
header_names <- gsub('NA_', '', paste(na.locf(unlist(modelrun100[1,]), na.rm = FALSE), unlist(modelrun100[2,]), sep = '_') )

modelrun100 <- modelrun100 %>%
  slice(-(1:2)) %>%
  setNames(header_names) %>%
  mutate_at(1:3, na.locf)

modelrun100_materials <- modelrun100 %>%
  filter(Cost_Type %in% 'Materials') %>%
  select(`Product Category`:`Brand Type`, Total_5th:Total_95th) %>%
  mutate_at(vars(Total_5th:Total_95th), as.numeric)

# Totals across all the food types
modelrun100_materials %>% select(Total_5th:Total_95th) %>% colSums # 0, because all of it is labor costs in this model.

# Low mean and high for no coordination and all coordination
datelabel_material_costs_nocoord <- modelrun0_materials %>% select(Total_5th:Total_95th) %>% colSums
datelabel_material_costs_allcoord <- modelrun100_materials %>% select(Total_5th:Total_95th) %>% colSums # zeroes.

# Spoilage prevention packaging -------------------------------------------

# For packaging, we are going to use one-time costs, then per-unit costs. 
# Mary has already done the cost totals. 
# The annualized initial cost is relatively small; most of it is the annual cost. We will represent 100% of the annual cost being materials.

# We now use ReFED's assumptions that this can be done for 15% of fruit and 25% of meat, with 10-33% retail waste reduction and 5-10% residential waste reduction.

packaging_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/packaging/costs for packaging development.xlsx'), skip = 10, col_names = c('cost_type_2014_dollars', 'low', 'mean', 'high', 'notes')) 

packaging_annual_equipment_costs <- packaging_costs[packaging_costs$cost_type_2014_dollars %in% 'Total annual costs', 2:4]
packaging_total_costs <- packaging_costs[packaging_costs$cost_type_2014_dollars %in% 'Total annualized and annual costs', 2:4]
packaging_initial_costs <- packaging_costs[packaging_costs$cost_type_2014_dollars %in% 'Annualized initial costs (5 years, 7%)', 2:4]

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
prop_processed_fruit <- prop_fruit_veg[2]/sum(prop_fruit_veg) # 85.16% by $ value of the processed fruit/veg industry is fruit.

food_U[fruit_rows, retail_codes] # This is basically zero. We will need to just change the final consumer demand by the product of the waste rates, and use consumer price.

##################
# EDIT ANYTHING BELOW TO INCLUDE FRUIT, MEAT, POULTRY, AND SEAFOOD
##################

##########
# LAFA rate conversion for the fruit and meat codes in LAFA.
# These are probably more than needed.
fruit_meat <- bea_codes %>% filter(stage %in% c('agriculture','processing'),
                                  fruit_veg_fresh > 0 | fruit_veg_processed > 0 | meat > 0 | fish_fresh > 0)

# We need baseline retail loss and consumer loss for fruits and meats

# Load LAFA
source(file.path(fp_github, 'fwe/read_data/read_lafa.r'))
lafa <- list(fruit, meat)

# Read the description of LAFA's nested category structure in.
lafa_struct <- read_csv(file.path(fp, 'crossreference_tables/lafa_category_structure.csv'))

# find the single year closest to 2012.
lafa_df <- lafa %>%
  map2_dfr(c('fruit','meat'), ~ select(.x, Category, Year, Retail_weight_Lbs.year, Loss_from_retail__institutional_to_consumer_level_Percent, Consumer_weight_Lbs.year,
                   Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent) %>%
             mutate(Group = .y)) %>%
  setNames(c('Food','Year','retail_weight','retail_loss','consumer_weight','avoidable_consumer_loss','Group')) %>%
  filter(!is.na(avoidable_consumer_loss)) %>%
  group_by(Group, Food) %>%
  filter(abs(Year - 2012) == min(abs(Year - 2012)))

lafa_df <- lafa_df %>% 
  ungroup %>%
  left_join(lafa_struct) %>%
  filter(subgroup2 == 'Fresh fruit' | subgroup1 %in% c('Red meat', 'Poultry', 'Total Fresh and Frozen Fish')) %>%
  mutate(group_final = if_else(Group == 'meat', subgroup1, subgroup2)) %>%
  select(group_final, Food:avoidable_consumer_loss)

fruitmeat_wtdavg_rates <- lafa_df %>%
  group_by(group_final) %>%
  summarize(retail_loss = weighted.mean(retail_loss, retail_weight),
            avoidable_consumer_loss = weighted.mean(avoidable_consumer_loss, consumer_weight))

# We now have the baseline retail and consumer waste rate for 4 groups: fresh fruit, poultry, seafood, and red meat.

### Load the baseline consumer demand for fresh fruit, red meat, poultry, and seafood in 2012.
# Do not include processed and frozen products, only fresh.
fruit_meat_codes <- c('111300', '311615', '31161A', '112A00', '114000') # fresh fruit, pkg poultry, pkg meat, aquaculture, wild caught fish
fruit_meat_proportions <- c(1, 1, 1, 0.113, 0.757) # the final two categories that contain seafood are not all seafood. (source: QCEW)

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
  mutate(food = c('fruit',rep('meat',4)), group_final = c('Fresh fruit', 'Poultry', 'Red meat', rep('Total Fresh and Frozen Fish', 2))) %>%
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

c(sum(fruitmeatdemand2012$demand_averted_lower), sum(fruitmeatdemand2012$demand_averted_upper))/1e9 # 1.27 to 2.91 billion dollars.

# Get full codes that will be recognized by USEEIO.
fruitmeatdemand2012 <- all_codes %>%
  select(sector_desc_drc, sector_code_uppercase) %>%
  right_join(fruitmeatdemand2012, by = c('sector_code_uppercase' = 'BEA_389_code'))

### 
# Run EEIO for the baseline, demand averted lower, and demand averted upper
# Do separately for each food so that we can see the result for each one.

if (!is_local) use_python('/usr/bin/python3')
source_python(file.path(fp_github, 'fwe/USEEIO/eeio_lcia.py'))

# 5 categories x (1 baseline+1averted lower+1averted upper) = 15 runs of model
# Already in units of dollars so don't need to multiply by any factor.
eeio_packaging_averted <- pmap(fruitmeatdemand2012, function(sector_desc_drc, baseline_demand, demand_averted_lower, demand_averted_upper, ...) {
  demand_code <- as.list(sector_desc_drc)
  list(baseline = eeio_lcia('USEEIO2012', as.list(baseline_demand), demand_code),
       averted_lower = eeio_lcia('USEEIO2012', as.list(demand_averted_lower), demand_code),
       averted_upper = eeio_lcia('USEEIO2012', as.list(demand_averted_upper), demand_code))
})

# Put this into a data frame
eeio_packaging_averted <- map2_dfr(c('fresh fruit', 'poultry', 'meat', 'farmed seafood', 'wild-caught seafood'), eeio_packaging_averted, function(x, y) {
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
         food = factor(food, levels = c('meat', 'poultry', 'fresh fruit', 'farmed seafood', 'wild-caught seafood'))) 


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

# dataframe %>% 
#   mutate_at(vars(contains('oo')), .funs = list(cat = ~ntile(., 2))) %>%


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
  summarize(net_lower = min(net_lower), upper = max(net_upper))

cost_per_reduction_packaging %>% 
  filter(grepl('enrg|eutr|gcc|land|watr', category)) %>%
  mutate(category = c('eutrophication ($/kg N)', 'greenhouse gas ($/kg CO2)', 'energy ($/MJ)', 'land ($/m2)', 'water ($/m3)'))
# Net result: 4 to 22 cents per kg CO2 averted. Compares favorably to WTA. Scale could be interesting too.
         