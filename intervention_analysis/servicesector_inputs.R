# Find all BEA industries that purchase any output of food industries.

library(tidyverse)

# Load lookup table and use table

fp_crosswalk <- ifelse(dir.exists('Q:/'), 'Q:/crossreference_tables', '/nfs/qread-data/crossreference_tables')
fp_bea <- ifelse(dir.exists('Q:/'), 'Q:/raw_data/BEA', '/nfs/qread-data/raw_data/BEA')

naics_lookup <- read.csv(file.path(fp_crosswalk, 'naics_crosswalk_rawfile11jun.csv'), stringsAsFactors = FALSE)

U <- read.csv(file.path(fp_bea, 'formatted/use2012.csv'), stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)

# Keep only the rows and columns of U that are in the industry table (get rid of all the extra sum rows, etc)
U <- U[rownames(U) %in% naics_lookup$BEA_389_code, colnames(U) %in% naics_lookup$BEA_389_code]

codes_level1 <- with(naics_lookup, BEA_389_code[substr(BEA_389_code,1,1) == "1" & food_system %in% c("partial", "y")])
codes_level3 <- with(naics_lookup, BEA_389_code[substr(BEA_389_code,1,1) == "3" & food_system %in% c("partial", "y")])
codes_level4 <- with(naics_lookup, BEA_389_code[substr(BEA_389_code,1,1) == "4" & food_system %in% c("partial", "y")])

naics_lookup %>% filter(substr(BEA_389_code,1,1) == "4" & food_system %in% c("partial", "y"))

U_sub <- U[rownames(U) %in% c(codes_level1, codes_level3, codes_level4), ]

inputs_level1 <- colSums(U[rownames(U) %in% codes_level1, ])
inputs_level3 <- colSums(U[rownames(U) %in% codes_level3, ])
inputs_level4 <- colSums(U[rownames(U) %in% codes_level4, ])

rowSums(U[rownames(U) %in% codes_level4, ])

U_bylevel <- rbind(inputs_level1, inputs_level3, inputs_level4)

# I think we can more or less ignore level 4.
U_bylevel <- rbind(inputs_level1, inputs_level3)
U_totalfood <- colSums(U_bylevel)

all_inputs <- naics_lookup[,1:2] %>% 
  left_join(data.frame(inputs = U_totalfood, BEA_389_code = names(U_totalfood))) %>%
  filter(inputs > 0) %>%
  arrange(-inputs)

# Filter this only including things in service sector and over $10M
inputs_servicesectors <- all_inputs %>%
  filter(!substr(BEA_389_code, 1, 1) %in% c('1','2','3'), inputs >= 10)

# Format table
inputs_servicesectors %>%
  mutate(inputs = if_else(inputs >= 1000, paste0('$', signif(inputs/1e3, 2), 'B'), paste0('$', signif(inputs, 3), 'M')))

U[rownames(U) %in% c(codes_level1, codes_level3), '441000', drop = FALSE]    # most of the input to vehicle sales is from 111400 so that might not really be food.     
