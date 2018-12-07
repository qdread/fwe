# Load codes from DRC and from USEEIO2012 final demand vector to make a lookup table to match all possible versions of the codes.
# QDR/FWE/07 Dec 2018

drc <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIO2012/USEEIO2012_DRC.csv', row.names = 1, check.names = FALSE)
dem <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv')

library(dplyr)

all_codes <- data.frame(sector_desc_drc = dimnames(drc)[[1]], stringsAsFactors = FALSE) %>%
  mutate(sector_code = substr(sector_desc_drc, 1, 6)) %>%
  left_join(with(dem, data.frame(sector_code = tolower(BEA_389_code), sector_code_uppercase = BEA_389_code, sector_desc_demand = BEA_389_def, stringsAsFactors = FALSE)))

write.csv(all_codes, '~/Dropbox/projects/foodwaste/Data/all_codes.csv', row.names = FALSE)
