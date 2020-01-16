# Cost effectiveness calculations for consumer education campaigns only
# QDR / FWE / 20 Nov 2019

# Refed's values for price per pound of food

library(tidyverse)

is_local <- dir.exists('Q:/')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')

# this csv is derived from pp 16 and 19 of the appendix.
refedpct <- read_csv(file.path(fp_crosswalk, 'refed_price_and_pct_tables.csv'))

# convert percents and dollars to numbers
refedpct[,-1] <- refedpct[,-1] %>% mutate_all(~ str_extract_all(., pattern = '[0-9.]+', simplify = TRUE) %>% as.numeric)
# Those are the percentages of waste. Percent targeted is not the same.

# The following numbers are on page 37 of the appendix
refedpct$percent_targeted <- c(16, 16, 43, 2, 23)

(div_pot <- 585e3 * 2000) # convert from tons to pounds, slightly over a billion

# Total final demand averted from households
demand_reduced_by_food <- div_pot * refedpct$Retail_price * refedpct$percent_targeted / 100

(demand_reduced <- sum(demand_reduced_by_food)) # I calculate $2.57B not $2.67B but close enough?

# Use USEEIO to determine the cost effectiveness of this program.
# We will put it divided proportionally among food retail establishments.

# Get gross outputs of the 5 food retail sectors and multiply by each one's food proportion.
retail_codes <- naics_foodsystem$BEA_389_code[naics_foodsystem$stage %in% 'retail']
retail_prop_food <- naics_foodsystem$proportion_food[naics_foodsystem$stage %in% 'retail']
gross_outputs <- M[retail_codes, 'T008'] * retail_prop_food

# Allocate demand proportionally to each of the gross outputs
(demand_reduced_by_sector <- setNames(demand_reduced * gross_outputs/sum(gross_outputs), retail_codes))

# Run USEEIO for these.

# Get the matching codes.
all_codes <- read.csv(file.path(fp_crosswalk, 'all_codes.csv'), stringsAsFactors = FALSE)
code_lookup <- all_codes %>%
  transmute(BEA_code = sector_code_uppercase, BEA_code_full = sector_desc_drc)

retail_names <- as.list(code_lookup$BEA_code_full[match(retail_codes, code_lookup$BEA_code)])
demand_list <- as.list(as.numeric(demand_reduced_by_sector))

# Calculate USEEIO
lcia_result <- eeio_lcia('USEEIO2012', demand_list, retail_names)

lcia_result <- data.frame(impact_category = row.names(lcia_result),
                          reduced_value = lcia_result$Total)

# Convert this into per capita
lcia_result <- lcia_result %>%
  mutate(reduced_percapita = reduced_value / 314e6)

# Compare with the per capita impacts of food waste across the entire food system.
waste_impacts <- read_csv(file.path(fp_output, 'baseline_impacts.csv'))

lcia_result_withbase <- lcia_result %>% left_join(waste_impacts)

final_props <- lcia_result_withbase %>% 
  mutate(proportion_reduced = reduced_value / waste_impact) %>%
  select(impact_category, proportion_reduced, reduced_value, waste_impact)

write_csv(final_props, file.path(fp_output, 'impact_reduction_consumer_campaigns_refed.csv'))
