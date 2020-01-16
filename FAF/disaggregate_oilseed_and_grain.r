# Code to disaggregate oilseeds and grains within each state, using NASS commodity totals

library(tidyverse)

# Read text file ---------------------------------------------------


cdqt_file <- '/nfs/qread-data/raw_data/USDA/2012_cdqt_data.txt'

cdqt <- read_delim(cdqt_file, delim = '\t', col_names = FALSE, col_types = strrep('c', 15))


# Get rid of duplicate rows -----------------------------------------------

# Ignore the numerical columns 1-4 and get duplicate rows for all other columns.

dup_idx <- duplicated(cdqt[,-(1:4)])

cdqt <- cdqt[!dup_idx, ]

# Locate rows with info on oilseeds and grains ----------------------------

# we want crop totals - sales, by state and crop. 

# 1111A0: 111110, 111120 - soybean,  canola, flaxseed, sesame, sunflower, rapeseed, safflower, mustard seed
# 1111B0: 111130, 111140, 111150, 111160, 111191, 111199: dry pea, bean, wheat, corn, rice, other grains (barley, milo, sorghum, broomcorn, oat, wild rice, buckwheat, rye)

cdqt_cropsales_oilseedgrain <- cdqt %>% 
  filter(grepl('CROP TOTALS - SALES', X6),
         grepl('NAICS CLASSIFICATION: (1111', X14, fixed = TRUE))

# Sum these up where 11111 and 11112 are summed as oilseeds, anything else summed as grains

unique(cdqt_cropsales_oilseedgrain$X14)

cdqt_cropsales_oilseedgrain <- cdqt_cropsales_oilseedgrain %>%
  select(X9, X10, X11, X14, X15) %>%
  setNames(c('state_fips', 'state_abbrev', 'state_name', 'NAICS', 'value')) %>%
  mutate(value = if_else(value == '(D)', as.character(NA), value),
         value = as.numeric(gsub('[[:punct:]]', '', value)),
         NAICS = gsub('NAICS CLASSIFICATION: ', '', NAICS),
         NAICS = gsub('\\(|\\)', '', NAICS))

cropsales_oilseed_vs_grain <- cdqt_cropsales_oilseedgrain %>%
  filter(!NAICS %in% '1111') %>%
  mutate(BEA_Code = if_else(NAICS %in% c('11111', '11112'), '1111A0', '1111B0')) %>%
  group_by(state_fips, state_abbrev, state_name, BEA_Code) %>%
  summarize(sales = sum(value, na.rm = TRUE))

cropsales_oilseed_vs_grain_wide <- cropsales_oilseed_vs_grain %>%
  pivot_wider(names_from = BEA_Code, values_from = sales, values_fill = list(sales = 0))


# Do by crop type directly ------------------------------------------------

# Because the sums do not quite match up, NAICS may not be the best thing to use.
# Try to find the crop total sales by crop type.


grains <- c('BARLEY','CORN','OATS','WHEAT','PEAS, DRY EDIBLE','BEANS, DRY EDIBLE','RICE','RYE ','SORGHUM','MILLET','LENTILS')
oilseeds <- c('SOYBEANS','SUNFLOWER','CANOLA','FLAXSEED','RAPESEED','SAFFLOWER','MUSTARD, SEED')

crop_idx <- map(c(grains, oilseeds), ~which(grepl(., cdqt$X6)))
crop_idx <- sort(Reduce(union, crop_idx))

databycrop <- cdqt[crop_idx,]

# filter crop data by production
databycrop <- databycrop %>%
  filter(grepl('PRODUCTION', X6), !X8 %in% 'COUNTY') %>%
  select(X6, X7, X9, X10, X11, X14, X15) %>%
  setNames(c('variable','crop','state_fips','state_abbrev','state_name','class','value')) %>%
  separate(variable, into = c('crop_long','variable'), sep = ' - ') %>%
  mutate(value = as.numeric(gsub('[[:punct:]]', '', value)))

# state and us totals
crop_totals <- databycrop %>% 
  filter(is.na(class)) %>% 
  select(-class) %>%
  filter(!grepl('SILAGE|SYRUP|GRASSES|SWEET CORN', crop_long)) %>%
  filter(!crop_long %in% 'BEANS, DRY EDIBLE, LIMA') %>%
  mutate(variable = gsub('PRODUCTION, MEASURED IN ', '', variable))

# determine what is the unit
table(crop_totals$crop_long, crop_totals$variable)
# hundredweight can be converted to lb easily, bushel must be converted to lb by crop

# Load the weights per bushel derived from https://www.rayglen.com/crop-bushel-weights/
wt_bushel <- read_csv(file.path(fp_crosswalk, 'wt_per_bushel.csv')) %>%
  mutate(crop = toupper(crop))
# Load crop prices derived from https://www.nass.usda.gov/Publications/Todays_Reports/reports/cpvl0217.pdf
prices <- read_csv(file.path(fp_crosswalk, 'crop_priceperunit_20142016.csv')) %>%
  mutate(crop = toupper(crop), variable = toupper(unit)) %>%
  group_by(crop, variable) %>%
  summarize(price = mean(price))

prices <- prices %>% 
  regex_left_join(wt_bushel, by = 'crop') %>%
  mutate(price_per_lb = if_else(variable == 'BU', price / lbs_per_bushel, price / 100))

prices_lb <- prices %>% select(crop.x, price_per_lb) %>% rename(crop_long = crop.x)

crop_totals <- crop_totals %>%
  left_join(wt_bushel) %>%
  mutate(production_lbs = case_when(
    variable == 'LB' ~ value,
    variable == 'CWT' ~ value * 100,
    variable == 'BU' ~ value * lbs_per_bushel
  ))

prices_lb$crop_long[2] <- 'BEANS, DRY EDIBLE, (EXCL LIMA), INCL CHICKPEAS'

crop_totals <- crop_totals %>%
  left_join(prices_lb) %>%
  mutate(value_dollars = production_lbs * price_per_lb)

# Correct so that no crops are double counted
crop_totals %>% group_by(crop) %>% summarize(n = length(unique(crop_long))) %>% print(n=nrow(.)) # Sunflower and wheat are the only ones this applies to

# Make sure each state has a valid value for sunflowers
crop_totals %>% filter(crop=='SUNFLOWER') %>%
  select(crop_long,state_name,value) %>%
  pivot_wider(names_from = crop_long) %>% 
  mutate(total = `SUNFLOWER, OIL TYPE` + `SUNFLOWER, NON-OIL TYPE`) %>%
  print(n=nrow(.))

# We can delete all rows with sunflower oil and non oil specified
crop_totals <- crop_totals %>%
  filter(!crop_long %in% c('SUNFLOWER, OIL TYPE','SUNFLOWER, NON-OIL TYPE'))

# Do the same for wheat.
crop_totals %>% filter(crop=='WHEAT') %>%
  select(crop_long,state_name,value) %>%
  pivot_wider(names_from = crop_long,values_fill=list(value=0)) %>% 
  mutate(total = `WHEAT, WINTER`+ `WHEAT, SPRING, DURUM`+ `WHEAT, SPRING, (EXCL DURUM)`,
         diff = WHEAT - total) %>%
  print(n=nrow(.))

# We can delete all rows with just wheat in it since we have the more specific values for the 3 types of wheat.
crop_totals <- crop_totals %>%
  filter(!crop_long %in% 'WHEAT')

# Check how well we have covered dollar values
crop_totals %>% filter(!is.na(value), is.na(value_dollars)) %>% print(n=nrow(.)) # These are small specialty crops and don't make a big difference

# Write this
write_csv(crop_totals, file.path(fp_out, 'crop_totals_for_oilseeds_and_grains.csv'))

# Assign each row to either grains or oilseeds
# Then sum up production by state and type
oilseed_grain_totals <- crop_totals %>%
  mutate(crop_class = if_else(crop %in% c(oilseeds, 'MUSTARD'), 'oilseed', 'grain')) %>%
  group_by(state_fips, state_abbrev, state_name, crop_class) %>%
  summarize(production_lbs = sum(production_lbs, na.rm = TRUE),
            value_dollars = sum(value_dollars, na.rm = TRUE)) 

oilseed_grain_proportions <- oilseed_grain_totals %>%
  select(-production_lbs) %>%
  pivot_wider(names_from = crop_class, values_from = value_dollars, values_fill = list(value_dollars = 0)) %>%
  mutate(proportion_grain = grain / (grain + oilseed),
         proportion_oilseed = oilseed / (grain + oilseed))

# Write final proportion by state
write_csv(oilseed_grain_proportions, file.path(fp_out, 'oilseed_grain_proportions.csv'))
