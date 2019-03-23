# see if cnpp matches any other data

library(XLConnect)
library(tidyverse)
library(sas7bdat)

cnpp <- readWorksheetFromFile('Z:/USDAnutrients/FoodPricesDatabase0304.XLS', sheet = 1, startRow = 3, header = FALSE) %>%
  setNames(c('foodcode', 'foodname','price0304'))

# Read SAS version of CNPP data
cnpp_sas <- read.sas7bdat('Z:/USDAnutrients/FoodPricesDatabase0304.sas7bdat') # Contains same rows but with prices for different years and for the 4 regions.

# read usda NDB codes
ndb <- read.csv('~/Documents/data/USDAnutrients/watercontent_combined.csv', stringsAsFactors = FALSE)

table(nchar(cnpp$foodcode))
table(nchar(ndb$NDB_NO))

# Read FDIC recipe file to see if we can break down the different foods into different commodities
recipes <- read.csv('Z:/FCID/Recipes_WWEIA_FCID_0510.csv')

table(cnpp$foodcode %in% recipes$Food_Code) # Almost all match.

table(table(recipes$Food_Code)) # Individual foods have between 1 and 1277 ingredients.

# Get FCID codes that go with the different food codes
fcid_code_desc <- read.csv('Z:/FCID/FCID_Code_Description.csv')
table(recipes$FCID_Code %in% fcid_code_desc$FCID_Code) # Mostly all there.

# More updated recipes: FNDDS
fndds_ingr <- readWorksheetFromFile('Z:/FNDDS/2015-2016 FNDDS At A Glance - FNDDS Ingredients.xlsx', sheet = 1, startRow = 2) # This is the one that is equivalent to the FCID
fndds_nutr <- readWorksheetFromFile('Z:/FNDDS/2015-2016 FNDDS At A Glance - FNDDS Nutrient Values.xlsx', sheet = 1) # Overflows memory to load this; convert to CSV first?

table(cnpp$foodcode %in% fndds_ingr$Food.code) # About 1000 do not match unfortunately.

head(cnpp$foodname[!cnpp$foodcode %in% fndds_ingr$Food.code])
