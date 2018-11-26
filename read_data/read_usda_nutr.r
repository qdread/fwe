# Explore standard nutrient content reference data
# Try to extract water content from it.

nutr <- read.csv('Q:/USDAnutrients/brandedfood/Nutrients.csv', stringsAsFactors = FALSE)
prods <- read.csv('Q:/USDAnutrients/brandedfood/Products.csv', stringsAsFactors = FALSE)

# Get list of all LAFA commodities
dairy_names <- unique(dairy$Category)
fat_names <- unique(fat$Category)
fruit_names <- unique(fruit$Category)
grain_names <- unique(grain$Category)
meat_names <- unique(meat$Category)
sugar_names <- unique(sugar$Category)
veg_names <- unique(veg$Category)

# Water content report produced from website
watercontent <- read.csv('Q:/USDAnutrients/watercontent.csv', stringsAsFactors = FALSE, skip = 6, header = TRUE)
# Approx 7K foods. We can match them up.

grep('apple', watercontent$Description, value = TRUE, ignore.case = TRUE)
