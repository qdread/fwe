# New clean code pipeline for assigning weights to NAICS sectors
# Assigns proportion food and nonfood to partial FSC sectors
# For each whole or partial FSC sector that includes more than one FAO loss category, assign relative weights to the FAO loss categories it contains.
# QDR / FWE / 11 June 2019

library(tidyverse)

# Load lookup table and use table

fp_crosswalk <- ifelse(dir.exists('Q:/'), 'Q:/crossreference_tables', '/nfs/qread-data/crossreference_tables')
fp_bea <- ifelse(dir.exists('Q:/'), 'Q:/raw_data/BEA', '/nfs/qread-data/raw_data/BEA')

naics_lookup <- read.csv(file.path(fp_crosswalk, 'naics_crosswalk_rawfile11jun.csv'), stringsAsFactors = FALSE)

U <- read.csv(file.path(fp_bea, 'formatted/use2012.csv'), stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)

# Proportions FSC by sector -----------------------------------------------

# For each of the food service and institutional categories, calculate the proportion of intermediate inputs for that sector that is from the "earlier" FSC stages.

# Modified 07 June 2019: use most up to date proportions from lookup table

# Get the index of the intermediate rows in the use table
U_commod_row <- dimnames(U)[[1]][1:(which(dimnames(U)[[1]] == 'T005') - 1)]

# Stages to be classified as early FSC stages to calculate proportion of inputs that are received by the institutional and food service sectors
early_fsc_stages <- c('agriculture', 'processing', 'retail')
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
naics_lookup$source_proportion_food[naics_lookup$food_system %in% 'partial' & naics_lookup$stage %in% c('foodservice', 'institutional')] <- 'IO table (assuming inputs proportional to outputs)'

write.csv(naics_lookup, file.path(fp_crosswalk, 'naics_crosswalk_rawfile11jun_sectorproportionfscadded.csv'))

# Weighting of FAO food categories within sectors ------------------------------

# Level 1 to Level 3 inputs

level3_fsc_sectors <- naics_lookup %>% filter(stage %in% 'processing') %>% pull(BEA_389_code)
level1_fsc_sectors <- naics_lookup %>% filter(stage %in% 'agriculture') %>% pull(BEA_389_code)
level1_prop_adjustment <- data.frame(sector = level1_fsc_sectors,
                                     proportion_food = naics_lookup$proportion_food[match(level1_fsc_sectors, naics_lookup$BEA_389_code)])


level1_inputs_tolevel3 <- U[level1_fsc_sectors, level3_fsc_sectors]
level1_inputs_tolevel3 <- sweep(level1_inputs_tolevel3, 2, colSums(level1_inputs_tolevel3), '/')

# Now find the proportion of each FAO category corresponding to the level 1 sectors that go into level 3.
# For each column of the matrix above, we want a row vector of length 12. Probably best to do it manually since there are some decisions to make.
# This will be done manually and written to a different file.
write.csv(level1_inputs_tolevel3, file.path(fp_crosswalk, 'level1_to_level3_inputs.csv'))

# Level 1 and 3 to levels 4 and above inputs

# Read the manually edited CSV in.
naics_lookup2 <- read.csv(file.path(fp_crosswalk, 'naics_crosswalk_rawfile11jun_level1inputsmanuallyadded.csv'), stringsAsFactors = FALSE)

later_sectors <- naics_lookup %>% filter(stage %in% c('foodservice', 'institutional', 'retail'), proportion_food > 0) %>% pull(BEA_389_code) # 39 later sectors

level1and3_inputs_tohigherlevels <- U[c(level1_fsc_sectors, level3_fsc_sectors), later_sectors]

# Added 10 Feb. 2020: write this level 1 and 3 to levels 4 and up matrix to a CSV for use in later analysis.
write.csv(level1and3_inputs_tohigherlevels, file.path(fp_crosswalk, 'level13_to_level4678_inputs.csv'))

faoproportions_forlevel1and3 <- naics_lookup2 %>% filter(stage %in% c('agriculture', 'processing')) %>% select(cereals:beverages)

# Multiply the matrices
faoproportions_forhigherlevels <- t(level1and3_inputs_tohigherlevels) %*% as.matrix(faoproportions_forlevel1and3)
faoproportions_forhigherlevels <- round(sweep(faoproportions_forhigherlevels, 1, rowSums(faoproportions_forhigherlevels), '/'), 3)


# For the ones that don't have explicit input from food sectors, apply a similar row to it for a correction.
# Assign warehousing the values for wholesale trade
faoproportions_forhigherlevels['493000',] <- faoproportions_forhigherlevels['420000',]
# Assign passenger ground transport the values for rail transport
faoproportions_forhigherlevels['485000',] <- faoproportions_forhigherlevels['482000',]
# Assign other education the values for college educational institutions
faoproportions_forhigherlevels['611B00',] <- faoproportions_forhigherlevels['611A00',]
# Assign "other" state government the values for state and local government
faoproportions_forhigherlevels['S00203',] <- faoproportions_forhigherlevels['S00700',]

# Replace the ones with the correct values in the lookup table.
col_idx <- which(names(naics_lookup2) == 'cereals'):which(names(naics_lookup2) == 'beverages')
naics_lookup2[match(later_sectors, naics_lookup2$BEA_389_code), col_idx] <- faoproportions_forhigherlevels

# Put the comma-separated strings in the lookup table.
commastrings <- apply(faoproportions_forhigherlevels, 1, function(x) paste0(which(x>0), collapse=','))
naics_lookup2[match(later_sectors, naics_lookup2$BEA_389_code), 'FAO_category'] <- commastrings
naics_lookup2[match(later_sectors, naics_lookup2$BEA_389_code), 'source_food_weighting'] <- 'IO table (assuming level 1 and 3 inputs proportional to outputs)'

# Sort it by stage.
naics_sorted <- naics_lookup2[order(factor(naics_lookup2$stage, levels = c('agriculture', 'processing', 'retail', 'foodservice', 'institutional'))),]

# Write output.
write.csv(naics_sorted, file.path(fp_crosswalk, 'naics_crosswalk_rawfile11jun_higherlevelinputscalculated.csv'), row.names = FALSE)
