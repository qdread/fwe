# Convert updated BEA 2007 and 2012 data to CSV (make and use tables)

options(java.parameters = "-Xmx4g") # Allows 4 gigs of ram to be used to load the XLSX workbooks.
library(XLConnect)

wb_use <- loadWorkbook('Q:/BEA/IOUse_Before_Redefinitions_PRO_DET.xlsx')
use07 <- readWorksheet(wb_use, '2007', startRow = 6, check.names = FALSE)
use12 <- readWorksheet(wb_use, '2012', startRow = 6, check.names = FALSE)

# Compare row names in old one. Also try to reconstruct USEEIO's input format with old data to make sure they match.
use_old_processed <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/SI/BEA/389_Use_2007_PRO_BeforeRedef.csv',row.names=1,check.names=F,nrows=389)
wb_use_old <- loadWorkbook('Q:/IO_tables/BEA2007/IOUse_Before_Redefinitions_PRO_2007_Detail.xlsx')
use_old_raw <- readWorksheet(wb_use_old, '2007', startRow = 6, check.names = FALSE)

use_old_processed_byme <- use_old_raw[1:389,]
dimnames(use_old_processed_byme)[[1]] <- use_old_processed_byme$Code
use_old_processed_byme <- use_old_processed_byme[,-(1:2)]

all.equal(use_old_processed, use_old_processed_byme)
all.equal(dimnames(use_old_processed), dimnames(use_old_processed_byme))

to_int <- function(df) sapply(df, as.integer)
all.equal(to_int(use_old_processed), to_int(use_old_processed_byme)) # These are the same so we can use the same method on the BEA data.
identical(to_int(use_old_processed), to_int(use_old_processed_byme)) # It gets rid of row names but that's OK for now.

all(use07$Code[1:389] == dimnames(use_old_processed)[[1]])
table(use07$Code %in% dimnames(use_old_processed)[[1]]) # There are a lot that do not match. Why is this.
table(dimnames(use_old_processed)[[1]] %in% use07$Code) # 367 match, 22 don't

old_not_in_new <- dimnames(use_old_processed)[[1]][!dimnames(use_old_processed)[[1]] %in% use07$Code]
new_not_in_old <- use07$Code[!use07$Code %in% dimnames(use_old_processed)[[1]]]

# Get the code and commodity description to see how closely they match.
old_nomatch <- use_old_raw[!use_old_raw$Code %in% use07$Code, c('Code', 'Commodity Description')]
new_nomatch <- use07[!use07$Code %in% use_old_raw$Code, c('Code', 'Commodity Description')]

# Write new no-match rows to a table.
# Manually create mapping to old rows for each of them.
# They can match a row from the old system that either is or isn't in the new system.

write.csv(new_nomatch, file = 'Q:/BEA/formatted/new_nomatch_codes.csv', row.names = FALSE)

# The most problematic codes are the ones beginning 42, 44, 45, as most of the rest are one to one or several to one (new-to-old) mappings.
old_problem_codes <- use_old_raw[substr(use_old_raw$Code, 1, 2) %in% c('42','44','45'), c('Code', 'Commodity Description')]
old_4_codes <- use_old_raw[substr(use_old_raw$Code, 1, 1) %in% c('4'), c('Code', 'Commodity Description')]

# However there are also a couple that aren't in the new scheme: copper smelting (331411), plastic and rubber machinery (333220), and office machinery (333313)
# Need to convert the new ones to those codes using the same proportion as the old.

new_old_mapping <- read.csv('Q:/BEA/formatted/new_to_old_mapping.csv', stringsAsFactors = FALSE)

# Determine the codes in the new categorization that need to be split to create the old categorization again.
old_nomatch$Code[!old_nomatch$Code %in% new_old_mapping$Code_old]
# Manually typed a CSV file for the splittings.

old_new_mapping <- read.csv('Q:/BEA/formatted/old_to_new_mapping.csv', stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------------

# Get rid of any rows past 'T008' row.
idx_max <- which(use07$Code == 'T008')

use07_out <- use07[1:idx_max, ]
dimnames(use07_out)[[1]] <- use07_out$Code
use07_out <- use07_out[,-(1:2)]

use12_out <- use12[1:idx_max, ]
dimnames(use12_out)[[1]] <- use12_out$Code
use12_out <- use12_out[,-(1:2)]

# -----------------------------------------------------------------------------------

# Use new to old mapping to put new data into old data format.
# We should end up with the same exact dimensions as use_old_processed (389R x 412C)
replacement_codes_rows <- new_old_mapping$Code_old[match(dimnames(use07_out)[[1]], new_old_mapping$Code_new)]
replacement_codes_cols <- new_old_mapping$Code_old[match(dimnames(use07_out)[[2]], new_old_mapping$Code_new)]
updated_rows <- ifelse(is.na(replacement_codes_rows), dimnames(use07_out)[[1]], replacement_codes_rows)
updated_cols <- ifelse(is.na(replacement_codes_cols), dimnames(use07_out)[[2]], replacement_codes_cols)

# -----------------------------------------------------------------------------------

# Programmatically do the mappings
source('read_data/reaggregate_mat.r')

codes_to_split <- old_new_mapping$Code_old[old_new_mapping$Code_old %in% updated_rows]
codes_to_split_new <- old_new_mapping$Code_new[old_new_mapping$Code_old %in% updated_rows]
code_split_keys <- lapply(codes_to_split_new, function(x) old_new_mapping$Code_old[old_new_mapping$Code_new == x])

# Use the raw use table to get the ratios.
row_totals <- lapply(code_split_keys, function(ks) use_old_raw[match(ks, use_old_raw$Code), 'T007'])
col_totals <- lapply(code_split_keys, function(ks) use_old_raw[which(use_old_raw$Code == 'T008'), ks])

use07_agg <- many_to_one(use07_out, updated_rows, updated_cols)
use07_agg_disagg <- one_to_many(use07_agg, codes_to_split, code_split_keys, row_totals, col_totals)

use12_agg <- many_to_one(use12_out, updated_rows, updated_cols)
use12_agg_disagg <- one_to_many(use12_agg, codes_to_split, code_split_keys, row_totals, col_totals)

write.csv(use07_agg_disagg, 'Q:/BEA/formatted/use2007.csv')
write.csv(use12_agg_disagg, 'Q:/BEA/formatted/use2012.csv')

# test equality
use07_read <- read.csv('Q:/BEA/formatted/use2007.csv', check.names = FALSE, row.names = 1)
all.equal(as.data.frame(use07_agg_disagg), use07_read) 

# -----------------------------------------------------------------------------------

# Now the make tables as well.
wb_make <- loadWorkbook('Q:/BEA/IOMake_Before_Redefinitions_DET.xlsx')
make07 <- readWorksheet(wb_make, '2007', startRow = 6, check.names = FALSE)
make12 <- readWorksheet(wb_make, '2012', startRow = 6, check.names = FALSE)

# For some reason the commas did not convert to numerics. Convert. Make sure not to convert the first two columns.
comma_to_num <- function(x) as.numeric(gsub(',', '', x))
for (i in 3:ncol(make07)) make07[,i] <- comma_to_num(make07[,i])
for (i in 3:ncol(make12)) make12[,i] <- comma_to_num(make12[,i])

# Compare row names in old one. Also try to reconstruct USEEIO's input format with old data to make sure they match.
make_old_processed <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/SI/BEA/389_Make_2007_PRO_BeforeRedef.csv',row.names=1,check.names=F,nrows=389)
make_old_processed_allrows <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/SI/BEA/389_Make_2007_PRO_BeforeRedef.csv',row.names=1,check.names=F)
wb_make_old <- loadWorkbook('Q:/IO_tables/BEA2007/IOMake_Before_Redefinitions_2007_Detail.xlsx')
make_old_raw <- readWorksheet(wb_make_old, '2007', startRow = 6, check.names = FALSE)

# -----------------------------------------------------------------------------------

make07_out <- make07
dimnames(make07_out)[[1]] <- make07_out$Code
make07_out <- make07_out[,-(1:2)]

make12_out <- make12
dimnames(make12_out)[[1]] <- make12_out$Code
make12_out <- make12_out[,-(1:2)]

# -----------------------------------------------------------------------------------

# Use new to old mapping to put new data into old data format.
# We should end up with the same exact dimensions as use_old_processed (389R x 412C)
replacement_codes_rows <- new_old_mapping$Code_old[match(dimnames(make07_out)[[1]], new_old_mapping$Code_new)]
replacement_codes_cols <- new_old_mapping$Code_old[match(dimnames(make07_out)[[2]], new_old_mapping$Code_new)]
updated_rows <- ifelse(is.na(replacement_codes_rows), dimnames(make07_out)[[1]], replacement_codes_rows)
updated_cols <- ifelse(is.na(replacement_codes_cols), dimnames(make07_out)[[2]], replacement_codes_cols)

# -----------------------------------------------------------------------------------

# Programmatically do the mappings
source('read_data/reaggregate_mat.r')

codes_to_split <- old_new_mapping$Code_old[old_new_mapping$Code_old %in% updated_rows]
codes_to_split_new <- old_new_mapping$Code_new[old_new_mapping$Code_old %in% updated_rows]
code_split_keys <- lapply(codes_to_split_new, function(x) old_new_mapping$Code_old[old_new_mapping$Code_new == x])

# Use the raw use table to get the ratios.
row_totals <- lapply(code_split_keys, function(ks) make_old_raw[match(ks, make_old_raw$Code), 'T008'])
col_totals <- lapply(code_split_keys, function(ks) make_old_raw[which(make_old_raw$Code == 'T007'), ks])

make07_agg <- many_to_one(make07_out, updated_rows, updated_cols)
make07_agg_disagg <- one_to_many(make07_agg, codes_to_split, code_split_keys, row_totals, col_totals)

make12_agg <- many_to_one(make12_out, updated_rows, updated_cols)
make12_agg_disagg <- one_to_many(make12_agg, codes_to_split, code_split_keys, row_totals, col_totals)

write.csv(make07_agg_disagg, 'Q:/BEA/formatted/make2007.csv')
write.csv(make12_agg_disagg, 'Q:/BEA/formatted/make2012.csv')
