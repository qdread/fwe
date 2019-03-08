# Extract information from CEX mean and SE data already generated from other BLS scripts
# QDR / FWE / 08 March 2019

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fpcex <- file.path(fp, 'CEX/CEX sample R output')

# combine all the regional data for each year
region_csvs <- dir(fpcex, pattern = 'region', full.names = TRUE)
cex_byregion <- map2_dfr(region_csvs, 2008:2017, ~ data.frame(year = .y, read.csv(.x, stringsAsFactors = FALSE)))

# Check parsing errors
cex_byregion %>% filter(is.na(group) | nchar(group) < 3)
cex_byregion %>% filter(nchar(group) < 3)

# Get water related expenditures
length(unique(cex_byregion$title))

cex_byregion %>% filter(grepl('Food',title))
