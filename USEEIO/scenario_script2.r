# More simplified scenario just altering final demand for plastic and fruits/vegs

library(purrr)

# Codes for each sector
plastic_film_code <- '326110'

freshfruit_code <- '111200'
freshveg_code <- '111300'
freshfv_code <- c(freshfruit_code, freshveg_code)

# Load original demand vector.
all_codes <- read.csv('~/Dropbox/projects/foodwaste/Data/all_codes.csv', stringsAsFactors = FALSE)
demand2012 <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv', stringsAsFactors = FALSE, check.names = FALSE)

total_freshfruitveg_consumerwt <- 113.90 + 155.19
total_freshfruitveg_available <- 50.54 + 84.79

consumerloss_proportion <- 1 - (total_freshfruitveg_available/total_freshfruitveg_consumerwt) # Approx 50%.

# Fruit and vegetable waste decreases
# Give a plausible number
fruitveg_finaldemand_decrease_vals <- seq(0, 100, 5) 

orig_demand <- demand2012$`2012_US_Consumption`[match(c(freshfv_code), demand2012$BEA_389_code)]
demand_change_proportions <- 1 - consumerloss_proportion * fruitveg_finaldemand_decrease_vals/100
demand_values <- orig_demand %*% t(demand_change_proportions) 

# Plastic demand increases
orig_demand_plasticfilm <- demand2012$`2012_US_Consumption`[match(c(plastic_film_code), demand2012$BEA_389_code)]
plastic_demand_increase_vals <- seq(0, 100, 5)
plastic_demand_values <- orig_demand_plasticfilm * (1 + plastic_demand_increase_vals/100)

# Convert demand_values to a list of vectors
demand_values <- map(seq_len(ncol(demand_values)), ~ demand_values[,.x]) 

# 5. Run the python script from R to calculate the LCIA results (and possibly other results) from the built model and demand vector.

# Source python script for getting model results
library(reticulate)
source_python('~/Documents/GitHub/fwe/USEEIO/eeio_lcia.py')

# Outer loop: models; inner loop: demand values
# For now we are assembling the model with every iteration but if this is too slow we can certainly change that.
scenario_grid <- expand.grid(model_path = model_names, demand_values = demand_values, stringsAsFactors = FALSE)