# Function to modify make and use tables to create different scenarios

# If we modify use table entry [i,j], that means we are changing the amount of product used by industry i to create the output of industry j.

make2012 <- read.csv('Q:/BEA/formatted/make2012.csv', row.names = 1, check.names = FALSE)
use2012 <- read.csv('Q:/BEA/formatted/use2012.csv', row.names = 1, check.names = FALSE)
# Last row and column of the make table, respectively, are the row and column sums of everything else in the table.
# The use table is more complex.

# ROWS:Sum of most of the elements of each row is in column t001. Sum of everything after that is in column t004. t001+t004=column t007
# COLUMNS:Sum of most elements is in row t005. Sum of the 3 after that is in row t006. t005+t006 = row t008.

rowtotal_colidx <- match(c('T001','T004','T007'), dimnames(use2012)[[2]]) # 390, 411, and 412
coltotal_rowidx <- match(c('T005', 'T006', 'T008'), dimnames(use2012)[[1]]) # 390, 394, 395

# Verify that these totals are correct within rounding error. (What I cannot create, I do not understand.)
t001 <- rowSums(use2012[1:389,1:(rowtotal_colidx[1] - 1)])
t004 <- rowSums(use2012[1:389,(rowtotal_colidx[1] + 1):(rowtotal_colidx[2] - 1)])
t007 <- t001 + t004

rowtotals_manual <- cbind(t001, t004, t007)
rowtotals_fromtable <- use2012[1:389,c('T001','T004','T007')]
rowtotals_diff <- rowtotals_manual - rowtotals_fromtable
hist(abs(rowtotals_diff[,1]))
hist(abs(rowtotals_diff[,2]))
hist(abs(rowtotals_diff[,3])) # A few of these values are a little off but it might be rounding error.

t005 <- colSums(use2012[1:(coltotal_rowidx[1] - 1), 1:389])
t006 <- colSums(use2012[(coltotal_rowidx[1] + 1):(coltotal_rowidx[2] - 1), 1:389])
t008 <- t005 + t006

coltotals_manual <- rbind(t005, t006, t008)
coltotals_fromtable <- use2012[c('T005','T006','T008'), 1:389]
coltotals_diff <- coltotals_manual - coltotals_fromtable
hist(t(abs(coltotals_diff[1,])))
hist(t(abs(coltotals_diff[2,])))
hist(t(abs(coltotals_diff[3,]))) # Very close agreement certainly within rounding error.


# Function to "retotal" formatted use table -------------------------------------

# Assumes we only change values within the primary sector cells of the use table (rows and cols 1-389)

retotal_usetable <- function(u) {
  # get row and col index of the total columns
  n <- which(dimnames(u)[[2]] == 'T001') - 1                     # Number of primary sectors in use table.
  u[1:n, 'T001'] <- apply(u[1:n, 1:n], 1, sum)                   # Redo intermediate row totals
  u[1:n, 'T007'] <- u[1:n, 'T001'] + u[1:n, 'T004']              # Redo grand row totals
  u['T005', ] <- apply(u[1:n, ], 2, sum)                         # Redo intermediate column totals
  u['T008', 1:(n+1)] <- u['T005', 1:(n+1)] + u['T006', 1:(n+1)]  # Redo grand column totals
  return(u)
}


# Example modification of table -------------------------------------------

use_mod <- use2012

# What if the cooking oil producing industry used less raw oilseed material because they reduced waste in their production process? (25% reduction)
oilseed_farming_code <- '1111A0'
oilseed_processing_code <- '31122A'
# Row i used by column j
use_mod[oilseed_farming_code, oilseed_processing_code]

use_mod[oilseed_farming_code, oilseed_processing_code] <- use_mod[oilseed_farming_code, oilseed_processing_code] * 0.75

use_mod <- retotal_usetable(use_mod)


# Define function to get cells of table -----------------------------------

# Takes vector of row codes and vector of column codes

get_cells <- function(m, row_codes, col_codes) {
  require(purrr)
  v <- map2_dbl(row_codes, col_codes, function(i, j) m[i, j])
  return(data.frame(row = row_codes, col = col_codes, val = v, stringsAsFactors = FALSE))
}

# Define function to change multiple cells of table -----------------------

# Input: a modified version of get_cells with a column for row, column for code, column for values

edit_cells <- function(m, newdata) {
  for (i in 1:nrow(newdata)) {
    m[newdata$row[i], newdata$col[i]] <- newdata$val[i]
  }
  return(m)
}

# Load EEIO sector classification to do modification ----------------------

eeio_sectors <- read.csv('~/Dropbox/projects/foodwaste/Data/useeio_sector_classification.csv', stringsAsFactors = FALSE)

# Create change factors for a particular scenario and use them to create a modified use table.

# Scenario 0. Do not change anything in the table but still retotal the use table so that we can make sure rounding is not an issue.
# Scenario 1. Vegetable oil production is 25% less wasteful of the input received from oilseed farming
# Scenario 2. All food processing sectors are 25% less wasteful of the input received from vegetable oil production
# Scenario 3. Food stores and restaurant sectors are 25% less wasteful of the input received by vegetable oil production

### Scenario 0

use_scen0 <- retotal_usetable(use2012)

### scenario 1

oilseed_farming_code <- '1111A0'
oilseed_processing_code <- '31122A'

scen1_values <- get_cells(use2012, oilseed_farming_code, oilseed_processing_code)
scen1_values$val <- scen1_values$val * 0.75

use_scen1 <- edit_cells(use2012, scen1_values)
use_scen1 <- retotal_usetable(use_scen1)

### scenario 2

processing_sector_codes <- eeio_sectors$BEA_389_code[eeio_sectors$stage == 'processing']

scen2_values <- get_cells(use2012, oilseed_processing_code, processing_sector_codes)
scen2_values$val <- scen2_values$val * 0.75

use_scen2 <- edit_cells(use2012, scen2_values)
use_scen2 <- retotal_usetable(use_scen2)

### scenario 3

retail_sector_codes <- eeio_sectors$BEA_389_code[eeio_sectors$stage == 'retail']

scen3_values <- get_cells(use2012, oilseed_processing_code, retail_sector_codes)
scen3_values$val <- scen3_values$val * 0.75

use_scen3 <- edit_cells(use2012, scen3_values)
use_scen3 <- retotal_usetable(use_scen3)

# Write new use tables to alternative scenario folder.
fpwrite <- 'Q:/BEA/scenario_usetables'

write.csv(use_scen0, file.path(fpwrite, 'use2012_scen0.csv'))
write.csv(use_scen1, file.path(fpwrite, 'use2012_scen1.csv'))
write.csv(use_scen2, file.path(fpwrite, 'use2012_scen2.csv'))
write.csv(use_scen3, file.path(fpwrite, 'use2012_scen3.csv'))
