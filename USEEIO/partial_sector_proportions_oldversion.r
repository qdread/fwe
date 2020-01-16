# Calculation of partial food system proportions for institutional and food service sectors
# Done simply by determining the proportion of inputs that are food-related, and assuming that is equal to the outputs
# QDR / FWE / 23 May 2019

library(tidyverse)

# Load lookup table and use table

fp_crosswalk <- ifelse(dir.exists('Q:/'), 'Q:/crossreference_tables', '/nfs/qread-data/crossreference_tables')
fp_bea <- ifelse(dir.exists('Q:/'), 'Q:/raw_data/BEA', '/nfs/qread-data/raw_data/BEA')

#naics_lookup <- read.csv(file.path(fp_crosswalk, 'naics_by_fao.csv'), stringsAsFactors = FALSE)
naics_lookup <- read.csv(file.path(fp_crosswalk, 'naics_crosswalk_allproportions_rawfile07june2019.csv'), stringsAsFactors = FALSE)

U <- read.csv(file.path(fp_bea, 'formatted/use2012.csv'), stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)


# Proportions FSC by sector -----------------------------------------------

# For each of the food service and institutional categories, calculate the proportion of intermediate inputs for that sector that is from the "earlier" FSC stages.

# Modified 07 June 2019: use most up to date proportions from lookup table

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
write.csv(naics_lookup, file = file.path(fp_crosswalk, 'naics_crosswalk_allproportions_rawfile07jun2019_v2.csv'), row.names = FALSE)


# Weighting of FAO food categories by sector ------------------------------

# For primary sectors, use QCEW.
# For all non-primary sectors, regardless of the proportion FSC, also get the weights for the food categories that go into each one.


library(tidyverse)
library(data.table)

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
qcew12 <- fread(file.path(fp, 'Census/QCEW/2012.annual.singlefile.csv'))

# Size code is not included
qcew_us <- qcew12 %>%
  filter(area_fips %in% 'US000') %>%
  select(own_code, industry_code, agglvl_code, annual_avg_estabs, annual_avg_emplvl)

qcew_us_ag <- qcew_us %>%
  filter(grepl('^(11)', industry_code))

# Fresh veg.
qcew_us_ag %>%
  filter(grepl('^(1112)', industry_code), agglvl_code == 18)

# Greenhouse.
qcew_us_ag %>%
  filter(grepl('^(1114)', industry_code), agglvl_code == 18)
qcew_us_ag %>%
  filter(grepl('^(1114)', industry_code), agglvl_code == 17)

# Tobacco, cotton, peanuts, and other crops.
qcew_us_ag %>%
  filter(grepl('^(1119)', industry_code), agglvl_code == 18) %>% group_by(industry_code) %>% summarize(emp = sum(annual_avg_emplvl))

# Poultry
qcew_us_ag %>%
  filter(grepl('^(1123)', industry_code), agglvl_code == 18) 

# Animal farms and aquaculture
qcew_us_ag %>%
  filter(grepl('^(112)', industry_code), agglvl_code == 18) %>% group_by(industry_code) %>% summarize(emp = sum(annual_avg_emplvl))

# Fish and game
qcew_us_ag %>%
  filter(grepl('^(114)', industry_code), agglvl_code == 18) %>% group_by(industry_code) %>% summarize(emp = sum(annual_avg_emplvl))

# Cattle meat production versus dairy production, to use the same ratio for other types of livestock.
# See above.


####
# For processed (level 3) food sectors, get weights based on ratios of the level 1 foods that are input into those sectors.

naics_lookup <- read.csv(file.path(fp_crosswalk, 'naics_crosswalk_allproportions_rawfile07jun2019_v2.csv'), stringsAsFactors = FALSE)

level3_fsc_sectors <- naics_lookup %>% filter(stage %in% 'processing') %>% pull(BEA_389_code)
level1_fsc_sectors <- naics_lookup %>% filter(stage %in% 'agriculture') %>% pull(BEA_389_code)
level1_prop_adjustment <- data.frame(sector = level1_fsc_sectors,
                                     proportion_food = naics_lookup$proportion_food[match(level1_fsc_sectors, naics_lookup$BEA_389_code)])


level1_inputs_tolevel3 <- U[level1_fsc_sectors, level3_fsc_sectors]
level1_inputs_tolevel3 <- sweep(level1_inputs_tolevel3, 2, colSums(level1_inputs_tolevel3), '/')

# Now find the proportion of each FAO category corresponding to the level 1 sectors that go into level 3.
# For each column of the matrix above, we want a row vector of length 12. Probably best to do it manually since there are some decisions to make.
write.csv(level1_inputs_tolevel3, file.path(fp_crosswalk, 'level1_to_level3_inputs.csv'))

#####
# For all the even higher level food sectors, get weights by multiplying all incoming level 1 and level 3 inputs by the proportions of FAO categoreis those inputs have (multiply 2 matrices)

naics_lookup2 <- read.csv(file.path(fp_crosswalk, 'naics_crosswalk_allproportions_rawfile07jun2019_v3.csv'), stringsAsFactors = FALSE)
later_sectors <- naics_lookup %>% filter(stage %in% c('foodservice', 'institutional', 'retail', 'wholesale'), proportion_food > 0) %>% pull(BEA_389_code)

level1and3_inputs_tohigherlevels <- U[c(level1_fsc_sectors, level3_fsc_sectors), later_sectors]
faoproportions_forlevel1and3 <- naics_lookup2 %>% filter(stage %in% c('agriculture', 'processing')) %>% select(cereals:sugar)

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
col_idx <- which(names(naics_lookup2) == 'cereals'):which(names(naics_lookup2) == 'sugar')
naics_lookup2[match(later_sectors, naics_lookup2$BEA_389_code), col_idx] <- faoproportions_forhigherlevels

# Put the comma-separated strings in the lookup table.
commastrings <- apply(faoproportions_forhigherlevels, 1, function(x) paste0(which(x>0), collapse=','))
naics_lookup2[match(later_sectors, naics_lookup2$BEA_389_code), 'FAO_category'] <- commastrings
naics_lookup2[match(later_sectors, naics_lookup2$BEA_389_code), 'source_food_weighting'] <- 'IO table'

# Write output.
write.csv(naics_lookup2, file.path(fp_crosswalk, 'naics_crosswalk_allproportions_rawfile07jun2019_v4.csv'), row.names = FALSE)
