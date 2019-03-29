# Get price per serving for different foods, using the crosswalk tables

library(tidyverse)


# Load data and crosswalks ------------------------------------------------

# Define file paths
fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fp_qfahpd <- file.path(fp, 'ERS/QFAHPD/tidy_data')
fp_usda <- file.path(fp, 'foods_consumption/USDAnutrients')
fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_fads <- file.path(fp, 'ERS/FADS/tidy_data')

# Load crosswalks
naicsCW <- read.csv(file.path(fp_crosswalks, 'naics_lafa_qfahpd_crosswalk_modified.csv'), stringsAsFactors = FALSE)
lafaCW <- read.csv(file.path(fp_crosswalks, 'lafa_qfahpd_naics_crosswalk.csv'), stringsAsFactors = FALSE)

# Load other "bridge" datasets
# CNPP food price data
cnpp <- readWorksheetFromFile(file.path(fp, 'food_consumption/USDAnutrients/FoodPricesDatabase0304.XLS'), sheet = 1, startRow = 3, header = FALSE) %>%
  setNames(c('foodcode', 'foodname','price0304'))
cnpp09 <- read.csv(file.path(fp, 'food_consumption/USDAnutrients/CNPP2009.csv'), stringsAsFactors = FALSE, na.strings = '.') %>%
  filter(!is.na(foodcode))
# FICRCD
fic08 <- read.csv(file.path(fp, 'food_consumption/FICRCD/corrected_ficrcd_2007_2008.csv'))
# FADS availability data
fruit_avail <- read.csv(file.path(fp_fads, 'fruit_availability.csv'), stringsAsFactors = FALSE)
veg_avail <- read.csv(file.path(fp_fads, 'veg_availability.csv'), stringsAsFactors = FALSE)
potato_avail <- read.csv(file.path(fp_fads, 'potato_availability.csv'), stringsAsFactors = FALSE)
fish_avail <- read.csv(file.path(fp_fads, 'fish_availability.csv'), stringsAsFactors = FALSE)
# QFAHPD version 2
qfahpd2 <- read.csv(file.path(fp_qfahpd, 'qfahpd2.csv'), stringsAsFactors = FALSE)

# Workflow

# Select all relevant NAICS codes

naicsCW %>% filter(stage %in% 'agriculture') # 11 codes
naicsCW %>% filter(stage %in% 'processing') # 26 codes (well 22, with 4 being drinks)
naicsCW %>% filter(stage %in% 'retail')
naicsCW %>% filter(food_system %in% c('y','partial')) # 4 retail and wholesale NAICS codes need to be applied to food system.



# Map each NAICS code to one or more LAFA categories and/or stages

# Use LAFA crosswalk table to map LAFA to FICRCD. Use FADS where needed to distinguish between different processing methods.

# Summarize the FADS availability ratios for processing methods for each of the foods
# ===================================================================================

# check that 2012 data exist for all years.
fruit_avail_mod <- fruit_avail %>%
  left_join(lafaCW, by = c('food' = 'FADS_food', 'type' = 'FADS_type')) %>%
  filter(year == 2012)

# For each column in FICRCD

# Map FICRCD to CNPP food price data
fic_cnpp_joined <- cnpp09 %>% rename(FoodCode = foodcode) %>% left_join(fic08)

# Why are there duplicates?
dupl_codes <- unique(fic_cnpp_joined$FoodCode[duplicated(fic_cnpp_joined$FoodCode)])
fic_cnpp_joined %>% filter(FoodCode == dupl_codes[2]) # Has something to do with the modification codes.

fic_cnpp_uniques <- fic_cnpp_joined %>% filter(!duplicated(FoodCode))

# Get the weighted average of the retail prices of the different foods. Weight this by the consumption of each one.
