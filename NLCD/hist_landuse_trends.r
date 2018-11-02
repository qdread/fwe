# Look at land-use trends in the historic landsat dataset, pooled by EPA level 3 ecoregion.
# QDR/FWE/31 Oct 2018

library(dplyr)
library(reshape2)
library(purrr)

hist_lu <- read.csv('Q:/USGS_LC/historic_nlcd_by_epa_l3_ecoregion.csv')

# To process:
# Add slivers to blocks
# Convert counts to areas or proportions
# Convert category codes to meaningful codes
# Take averages by ecoregion

data_dict <- c('0' = 'unclassified',
               '1' = 'water',
               '2' = 'developed',
               '3' = 'mechanically disturbed',
               '4' = 'mining',
               '5' = 'barren',
               '6' = 'forest',
               '7' = 'grassland/shrubland',
               '8' = 'agriculture',
               '9' = 'wetland',
               '10' = 'non-mechanically disturbed',
               '11' = 'ice and snow')

natural_codes <- c(5, 6, 7, 9, 10)
ag_codes <- 8
devel_codes <- c(2, 3, 4)
water_codes <- c(1, 11)
terr_codes <- c(natural_codes, ag_codes, devel_codes)

category_dict <- data.frame(category = 0:11, 
                            class = c('none', 'water', 'developed', 'developed', 'developed', 'natural', 'natural', 'natural', 'agriculture', 'natural', 'natural', 'water'),
                            name = data_dict) %>%
  mutate(terrestrial = class %in% c('developed', 'natural', 'agriculture'))

hist_lu_blocks <- hist_lu %>%
  filter(category != 0) %>% # Gets rid of no-data regions
  mutate(area = count * (60/1000)^2) %>% # Area in sq km
  group_by(L1_KEY, L2_KEY, US_L3NAME, US_L3CODE, block, year, category) %>% # Group sliver with its block
  summarize(area = sum(area))

hist_lu_sums <- hist_lu_blocks %>%
  left_join(category_dict) %>%
  filter(terrestrial) %>%
  mutate(natural = class == 'natural') %>% # Get only terrestrial natural and non-natural land area
  group_by(L1_KEY, L2_KEY, US_L3NAME, US_L3CODE, block, year, natural) %>%
  summarize(area = sum(area))

# Create a matrix by year for natural versus not natural.
# Need to include missing years.
years <- 1972:2002

tabulate_by_year <- function(dat) {
  props <- dat$proportion[match(years, dat$year)]
  data.frame(year = years, proportion = props)
}

hist_lu_sums_wide <- hist_lu_sums %>%
  mutate(proportion = area/sum(area)) %>%
  ungroup %>%
  filter(natural, !is.na(year)) %>%
  group_by(L1_KEY, L2_KEY, US_L3NAME, US_L3CODE, block) %>%
  do(p = tabulate_by_year(.))

yearmat <- do.call(cbind, map(hist_lu_sums_wide$p, 'proportion'))  

# There are a lot of missing values.
# Do a backward fill, then do a forward fill for anything still NA.

yearmat_fill <- yearmat %>%
  na.locf(fromLast = TRUE, na.rm = FALSE) %>% # backward fill
  na.locf(na.rm = FALSE) # forward fill

yeardf <- yearmat_fill %>%
  t %>%
  as.data.frame %>%
  setNames(paste('area', years, sep = '_'))

hist_lu_sums_wide <- hist_lu_sums_wide %>%
  select(-p) %>%
  cbind(yeardf)

hist_lu_byregion <- hist_lu_sums_wide %>%
  group_by(US_L3NAME, US_L3CODE) %>%
  summarize_at(vars(starts_with('area')), mean)

hist_lu_byregion_long <- hist_lu_byregion %>%
  melt(id.vars = c('US_L3NAME', 'US_L3CODE'), value.name = 'area', variable.name = 'year') %>%
  mutate(year = as.numeric(gsub('area_', '', year)))

library(ggplot2)

hist_lu_byregion_long <- hist_lu_byregion_long %>%
  group_by(US_L3NAME) %>%
  mutate(rel_prop = area/max(area)) %>%
  ungroup
ggplot(hist_lu_byregion_long, aes(x=year, y=rel_prop, group=US_L3NAME, color=US_L3NAME)) + geom_line()
