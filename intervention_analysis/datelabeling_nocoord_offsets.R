# Materials costs and impacts for date labeling change if we assume 0% coordination
# This was part of the main analysis but has been removed since we now assume 100% coordination and therefore no new 


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

# Annualization
(datelabel_material_costs_nocoord_annual <- pmt(datelabel_material_costs_nocoord, r = 0.07, n = 5, f = 0, t = 0)) # 80M to 192M.

###################################################

# Run EEIO for offset

# Offsetting impacts, using the lower, mean, and upper bounds of annualization of materials costs.
# Industries for this should be food packaging and labeling machinery - assign the entire one time cost to this.

pkg_machinery_code <- all_codes$sector_desc_drc[all_codes$sector_code_uppercase == '333993']

datelabeling_offset_eeio <- eeio_lcia('USEEIO2012', list(1), list(pkg_machinery_code))
datelabeling_offset_eeio <- data.frame(category = row.names(datelabeling_offset_eeio), 
                                       outer(datelabeling_offset_eeio[,'Total'], datelabel_material_costs_nocoord_annual)) %>%
  setNames(c('category', 'offset_lower', 'offset_mean', 'offset_upper'))

##################################################

# Combine impact and offset to get net impact reduced
eeio_datelabeling_result <- eeio_datelabeling %>% 
  pivot_wider(names_from = scenario, values_from = impact) %>%
  left_join(datelabeling_offset_eeio) %>%
  mutate(net_averted_mean_coordination = impact_averted_mean,
         net_averted_lower_coordination = impact_averted_lower,
         net_averted_upper_coordination = impact_averted_upper,
         net_averted_mean_nocoordination = impact_averted_mean - offset_mean,
         net_averted_lower_nocoordination = impact_averted_lower - offset_upper,
         net_averted_upper_nocoordination = impact_averted_upper - offset_lower,
         net_percent_averted_mean_coordination = 100 * net_averted_mean_coordination / impact_baseline,
         net_percent_averted_lower_coordination = 100 * net_averted_lower_coordination / impact_baseline,
         net_percent_averted_upper_coordination = 100 * net_averted_lower_coordination / impact_baseline,
         net_percent_averted_mean_nocoordination = 100 * net_averted_mean_nocoordination / impact_baseline,
         net_percent_averted_lower_nocoordination = 100 * net_averted_lower_nocoordination / impact_baseline,
         net_percent_averted_upper_nocoordination = 100 * net_averted_upper_nocoordination / impact_baseline)

# Cost per unit reduction for date labeling, using annualization of one-time costs
eeio_datelabeling_result <- eeio_datelabeling_result %>%
  mutate(cost_per_reduction_mean_coordination = datelabel_costs_coord_annual['mean'] / net_averted_mean_coordination,
         cost_per_reduction_lower_coordination = datelabel_costs_coord_annual['lower'] / net_averted_upper_coordination,
         cost_per_reduction_upper_coordination = datelabel_costs_coord_annual['upper'] / net_averted_lower_coordination,
         cost_per_reduction_mean_nocoordination = datelabel_costs_nocoord_annual['mean'] / net_averted_mean_nocoordination,
         cost_per_reduction_lower_nocoordination = datelabel_costs_nocoord_annual['lower'] / net_averted_upper_nocoordination,
         cost_per_reduction_upper_nocoordination = datelabel_costs_nocoord_annual['upper'] / net_averted_lower_nocoordination)