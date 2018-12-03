# Look at CFS data

library(dplyr)
library(data.table)
library(units)

cfs <- fread('Q:/CFS/cfs_2012_pumf_csv.txt')

# calculate total values
cfs <- cfs %>%
  mutate(total_value = SHIPMT_VALUE * WGT_FACTOR,
         total_weight = SHIPMT_WGHT * WGT_FACTOR %>% set_units(lb) %>% set_units(kg) %>% as.numeric)

#### Create state by state matrices for total value and total weight

# First sum up by origin and destination.
cfs_totals <- cfs %>%
  group_by(ORIG_STATE, DEST_STATE) %>%
  summarize(value = sum(total_value), mass = sum(total_weight))
# Note: a few have originating state of zero for confidentiality reasons but I guess we should ignore this?

sum(cfs_totals$value[cfs_totals$ORIG_STATE == 0])/sum(cfs_totals$value) # A very tiny proportion (less than 0.01%)
sum(cfs_totals$mass[cfs_totals$ORIG_STATE == 0])/sum(cfs_totals$mass)

# Next convert to square matrix.
# This can now be done with tidyr instead of reshape2 but probably about the same for each.

totals2square <- function(dat, x_col, y_col, value_col) {
  x_col <- enquo(x_col)
  y_col <- enquo(y_col)
  value_col <- enquo(value_col)
  mat <- dat %>%
    select(!!x_col, !!y_col, !!value_col) %>%
    spread(!!y_col, !!value_col) %>%
    as.matrix
  dimnames(mat)[[1]] <- mat[,1]
  mat[is.na(mat)] <- 0
  mat[,-1]
}

value_mat <- cfs_totals %>%
  filter(ORIG_STATE != 0) %>%
  totals2square(ORIG_STATE, DEST_STATE, value)

mass_mat <- cfs_totals %>%
  filter(ORIG_STATE != 0) %>%
  totals2square(ORIG_STATE, DEST_STATE, mass)

library(circlize)
chordDiagram(value_mat)

# Pull out some of the codes that have to do with the food system
# 01 live animals, 02 cereal grains, 03 other ag products, 04 animal feed, eggs, honey, other animal products, 05 meat 
# 06 milled grain and breads, 07 other prepared food, oil 08 alcoholic beverages
