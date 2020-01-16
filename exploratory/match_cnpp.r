# see if cnpp matches any other data

# "Venn diagram" numbers for 2 vectors.
compare <- function(a, b) {
  c(length(setdiff(a, b)), length(intersect(a, b)), length(setdiff(b, a)))
}

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')

library(XLConnect)
library(tidyverse)
library(sas7bdat)

cnpp <- readWorksheetFromFile(file.path(fp, 'food_consumption/USDAnutrients/FoodPricesDatabase0304.XLS'), sheet = 1, startRow = 3, header = FALSE) %>%
  setNames(c('foodcode', 'foodname','price0304'))
cnpp09 <- read.csv(file.path(fp, 'food_consumption/USDAnutrients/CNPP2009.csv'), stringsAsFactors = FALSE, na.strings = '.') %>%
  filter(!is.na(foodcode))

# Correct the non numeric value of cnpp 2009
# cnpp09 %>% filter(is.na(as.numeric(price_09))) # It's human milk and alcoholic beverages only.

# Read SAS version of CNPP data
# cnpp_sas <- read.sas7bdat('Q:/raw_data/USDAnutrients/FoodPricesDatabase0304.sas7bdat') # Contains same rows but with prices for different years and for the 4 regions.

# Do the two CNPP match?
table(cnpp$foodcode %in% cnpp09$foodcode)
table(cnpp09$foodcode %in% cnpp$foodcode)
# A lot of them do not.

# read usda NDB codes
ndb <- read.csv(file.path(fp, 'food_consumption/USDAnutrients/watercontent_combined.csv'), stringsAsFactors = FALSE)

table(nchar(cnpp$foodcode))
table(nchar(ndb$NDB_NO))

# Read FCID recipe file to see if we can break down the different foods into different commodities
recipes <- read.csv(file.path(fp, 'food_consumption/FCID/Recipes_WWEIA_FCID_0510.csv'))

table(cnpp$foodcode %in% recipes$Food_Code) # Almost all match.
table(cnpp09$foodcode %in% recipes$Food_Code) # these actually do match! 

table(recipes$Ingredient_Num) # Individual foods have between 1 and 55 ingredients.

# Get FCID codes that go with the different food codes
fcid_code_desc <- read.csv(file.path(fp, 'food_consumption/FCID/FCID_Code_Description.csv'))
table(recipes$FCID_Code %in% fcid_code_desc$FCID_Code) # Mostly all there.

# More updated recipes: FNDDS
fndds_ingr <- readWorksheetFromFile(file.path(fp, 'food_consumption/FNDDS/2015-2016 FNDDS At A Glance - FNDDS Ingredients.xlsx'), sheet = 1, startRow = 2) # This is the one that is equivalent to the FCID
fndds_nutr <- readWorksheetFromFile(file.path(fp, 'food_consumption/FNDDS/2015-2016 FNDDS At A Glance - FNDDS Nutrient Values.xlsx'), sheet = 1) # Overflows memory to load this; convert to CSV first?

table(cnpp$foodcode %in% fndds_ingr$Food.code) # About 1000 do not match unfortunately.
compare(cnpp$foodcode, fndds_ingr$Food.code)

compare(ndb$NDB_NO, fndds_ingr$Ingredient.code) # A lot of these match.

head(cnpp$foodname[!cnpp$foodcode %in% fndds_ingr$Food.code])

# Match with FICRCD
fic08 <- read.csv(file.path(fp, 'food_consumption/FICRCD/corrected_ficrcd_2007_2008.csv'))
table(cnpp09$foodcode %in% fic08$FoodCode)
table(cnpp$foodcode %in% fic08$FoodCode)

# ficrcd vs fcid
compare(recipes$Food_Code, fic08$FoodCode) # Most of the food codes in FICRCD are in FCID.

# Match with the newly created CSV files from FNDDS
fndds_files <- dir(file.path(fp, 'food_consumption/FNDDS/csvs'), full.names = TRUE)
fndds <- map(fndds_files, read.csv, stringsAsFactors = FALSE)
names(fndds) <- c('foods', 'ingredients_nutrients', 'ingredients', 'nutrients', 'weights')
