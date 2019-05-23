# Calculation of partial food system proportions for institutional and food service sectors
# Done simply by determining the proportion of inputs that are food-related, and assuming that is equal to the outputs
# QDR / FWE / 23 May 2019

library(tidyverse)

# Load lookup table and use table

fp_crosswalk <- ifelse(dir.exists('Q:/'), 'Q:/crossreference_tables', '/nfs/qread-data/crossreference_tables')
fp_bea <- ifelse(dir.exists('Z:/'), 'Z:/BEA', '/nfs/fwe-data/BEA')

naics_lookup <- read.csv(file.path(fp_crosswalk, 'naics_by_fao.csv'), stringsAsFactors = FALSE)
U <- read.csv(file.path(fp_bea, 'formatted/use2012.csv'), stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)

# For each of the food service and institutional categories, calculate the proportion of intermediate inputs for that sector that is from the "earlier" FSC stages.

# Get the index of the intermediate rows in the use table
U_commod_row <- dimnames(U)[[1]][1:(which(dimnames(U)[[1]] == 'T005') - 1)]

# Stages to be classified as early FSC stages to calculate proportion of inputs that are received by the institutional and food service sectors
early_fsc_stages <- c('agriculture', 'processing', 'wholesale', 'retail')
early_fsc_sectors <- naics_lookup %>% filter(stage %in% early_fsc_stages) %>% pull(BEA_389_code) # 36 sectors

# For each column of the use table, calculate the proportion received from "early" FSC stages
# Adjust this to account for the fact that some of the early sectors are not 100% food.

proportion_adjustments <- naics_lookup$proportion_food[match(early_fsc_sectors, naics_lookup$BEA_389_code)]
prop_fsc_input <- apply(U, 2, function(x) sum(x[early_fsc_sectors] * proportion_adjustments)/sum(x[U_commod_row]))

prop_fsc_input_df <- data.frame(BEA_389_code = names(prop_fsc_input), prop_fsc = prop_fsc_input)

# Get names of institutional sectors
institutional_sectors <- naics_lookup %>% filter(stage %in% 'institutional') %>% pull(BEA_389_code)
# Get names of food service sectors **not restaurants because those are all food sold**
foodservice_sectors <- naics_lookup %>% filter(food_system %in% 'partial', stage %in% 'foodservice') %>% pull(BEA_389_code)
prop_fsc_input_inst_and_foodservice <- prop_fsc_input_df %>% filter(BEA_389_code %in% c(institutional_sectors, foodservice_sectors))


# Join the new proportion list with the original df.
newprop_ordered <- naics_lookup %>%
  select(BEA_389_code) %>% 
  left_join(prop_fsc_input_inst_and_foodservice)

naics_lookup$proportion_food <- pmax(newprop_ordered$prop_fsc, naics_lookup$proportion_food, na.rm = TRUE)
naics_lookup$source_proportion_food[naics_lookup$food_system %in% 'partial' & naics_lookup$stage %in% c('foodservice', 'institutional')] <- 'IO table'
  
# Write result.
write.csv(naics_lookup, file = file.path(fp_crosswalk, 'naics_crosswalk_allproportions_rawfile.csv'), row.names = FALSE)

