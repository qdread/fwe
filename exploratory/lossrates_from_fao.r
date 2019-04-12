# Weighting NAICS codes by FAO loss rates to get baseline loss rate
# Convert this to get new loss rate for each scenario

fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
faopct <- readWorksheetFromFile(file.path(fp_crosswalks, 'fao_percentages.xlsx'), sheet = 1)
naicsCW <- read.csv(file.path(fp_crosswalks, 'naics_lafa_qfahpd_crosswalk_modified.csv'), stringsAsFactors = FALSE)



naics_foodsystem <- naicsCW %>% 
  filter(food_system %in% c('partial', 'y'), nchar(FAO_category) > 0) %>%
  arrange(stage) %>%
  mutate(proportion_food = if_else(is.na(proportion_food), 1, proportion_food)) %>%
  mutate(stage_code = case_when(
    stage %in% 'agriculture' ~ 'L1',
    stage %in% 'processing' ~ 'L2',
    TRUE ~ 'L3'
  ))

# Parse the FAO category comma separated lists of numbers into vectors.
FAO_category_vectors <- sapply(naics_foodsystem$FAO_category, function(x) {
  if (nchar(x) == 0) return(NA)
  eval(parse(text = paste0('c(',x,')')))
})

# Get baseline waste rate for each one
baseline_waste_rate <- map2_dbl(FAO_category_vectors, 1:nrow(naics_foodsystem), ~ mean(faopct[.x, naics_foodsystem$stage_code[.y]])) %>% as.numeric

# Function to calculate factor of demand change you get by reducing waste by a certain amount
# w_orig is original waste
# r is percent reduction in waste
demand_change_fn <- function(w_orig, r) (1 - w_orig) / (1 - (1 - r) * w_orig)

reduce25pct_waste_rate <- baseline_waste_rate * 0.25
reduce25pct_demand_factor <- demand_change_fn(baseline_waste_rate, 0.25)

naics_foodsystem <- naics_foodsystem %>%
  mutate(baseline_waste_rate = baseline_waste_rate,         reduce25pct_waste_rate = reduce25pct_waste_rate,
         reduce25pct_demand_factor = reduce25pct_demand_factor)

write.csv(naics_foodsystem, file = '/nfs/qread-data/scenario_inputdata/lossrates_by_naics.csv', row.names = FALSE)

