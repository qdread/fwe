# Extract information from CEX mean and SE data already generated from other BLS scripts
# QDR / FWE / 08 March 2019

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fpcex <- file.path(fp, 'CEX/csv_output')

# combine all the regional data for each year
region_csvs <- dir(fpcex, pattern = 'region', full.names = TRUE)
cex_byregion <- map2_dfr(region_csvs, 2008:2017, ~ data.frame(year = .y, read.csv(.x, stringsAsFactors = FALSE)))

# Check parsing errors
cex_byregion %>% filter(is.na(group) | nchar(group) < 3)

cex_byregion <- cex_byregion %>%
  filter(!(is.na(group) | nchar(group) < 3))

# Combine all the income class data for each year
incomeclass_csvs <- dir(fpcex, pattern = 'income', full.names = TRUE)
cex_byincomeclass <- map2_dfr(incomeclass_csvs, 2008:2017, ~ data.frame(year = .y, read.csv(.x, stringsAsFactors = FALSE)))

# Check parsing errors
cex_byincomeclass %>% filter(is.na(group) | nchar(group) < 3)

cex_byincomeclass <- cex_byincomeclass %>%
  filter(!(is.na(group) | nchar(group) < 3))

# Write output
write.csv(cex_byincomeclass, file.path(fp, 'CEX/final_data/cex_incomeclass.csv'), row.names = FALSE)
write.csv(cex_byregion, file.path(fp, 'CEX/final_data/cex_region.csv'), row.names = FALSE)
