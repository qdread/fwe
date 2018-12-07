# R script to run the python portion of the USEEIO workflow
# This will allow the entire workflow to be run from inside R without having to take a detour in python.

library(reticulate)
source_python('~/Documents/GitHub/fwe/USEEIO/eeio_lcia.py')

# Run a test
test_values <- c(1, 1)
test_codes <- c('1111b0/fresh wheat, corn, rice, and other grains/us', '111200/fresh vegetables, melons, and potatoes/us')
test_result <- eeio_lcia(model_path = 'USEEIO2012_scen0', demand_values = test_values, demand_codes = test_codes) # Works perfectly!

# Load the codes and the demand file and put together a scenario based on it.
all_codes <- read.csv('~/Dropbox/projects/foodwaste/Data/all_codes.csv', stringsAsFactors = FALSE)
demand2012 <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv', stringsAsFactors = FALSE, check.names = FALSE)

idx <- 2:4
test_values2 <- demand2012$`2012_US_Consumption`[idx]
test_codes2 <- all_codes$sector_desc_drc[match(demand2012$BEA_389_code[idx], all_codes$sector_code_uppercase)]
test_result2 <- eeio_lcia(model_path = 'USEEIO2012_scen1', demand_values = test_values2, demand_codes = test_codes2)
