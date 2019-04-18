# EEIO Scenario Master Script
# ---------------------------

# Currently do not wrap this into a function. Just show the workflow.

# There are two relevant user inputs.
# One is the structure of the DRC matrix itself. The user inputs a csv with 3 columns: the sector providing output, the sector receiving input, and the increment by which the cell should be changed relative to the 2012 baseline. If no change, input nothing.
# Two is the final demand vector. The user inputs a csv with 2 columns: the sector to be changed and the increment by which it should be changed relative to the 2012 baseline. If no change, input nothing.

# Workflow summary
# ----------------

# 1. Load the 2012 baseline use table and edit it based on user's input.
# 2. Write the edited use table to a csv.
# 3. Build USEEIO with the new table and write the resulting files to a directory.
# 4. Create a demand vector with the user's input.
# 5. Run the python script from R to calculate the LCIA results (and possibly other results) from the built model and demand vector.
# 6. Delete the intermediate files (edited use table and model build folder) unless user specifies not to delete.

# Required functions
# ------------------

# Define function to get cells of table -----------------------------------

# Takes vector of row codes and vector of column codes

get_cells <- function(m, row_codes, col_codes) {
  require(purrr)
  row_x_col <- cross2(row_codes, col_codes)
  v <- map_dbl(row_x_col, ~ m[.x[[1]], .x[[2]]])
  return(data.frame(row = map_chr(row_x_col, 1), col = map_chr(row_x_col, 2), val = v, stringsAsFactors = FALSE))
  #v <- map2_dbl(row_codes, col_codes, function(i, j) m[i, j])
  #return(data.frame(row = row_codes, col = col_codes, val = v, stringsAsFactors = FALSE))
}

# Define function to change multiple cells of table -----------------------

# Input: a modified version of get_cells with a column for row, column for code, column for values

edit_cells <- function(m, newdata) {
  for (i in 1:nrow(newdata)) {
    m[newdata$row[i], newdata$col[i]] <- newdata$val[i]
  }
  return(m)
}

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


# Corrected function to retotal the formatted use table -------------------

# This one explicitly takes the names from a separately created lookup table that I made, then sums them to get the correct values.



# Code
# ----

library(dplyr)

# 1. Load the 2012 baseline use table and edit it based on user's input.
# ----------------------------------------------------------------------

# Set file paths of BEA data and crosswalks
fp_bea <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'BEA')
fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')


# Load table
use2012 <- read.csv(file.path(fp_bea, 'formatted/use2012.csv'), row.names = 1, check.names = FALSE)

# Load sectors
eeio_sectors <- read.csv(file.path(fp_crosswalks, 'naics_lafa_qfahpd_crosswalk_modified.csv'), stringsAsFactors = FALSE)

# Example of change
# Get all stages
agriculture_stages <- eeio_sectors %>% filter(stage %in% 'agriculture')
processing_stages <- eeio_sectors %>% filter(stage %in% 'processing')
retail_stages <- eeio_sectors %>% filter(stage %in% 'retail')

# Get code corresponding to packaging sectors
packaging_machinery_code <- '333993'
plastic_film_code <- '326110'

# What percent of the demand for packaging machinery is taken up by the fruit and vegetable preservation sector?
fruitveg_preservation_code <- '311420'
freshfruit_code <- '111200'
freshveg_code <- '111300'
freshfv_code <- c(freshfruit_code, freshveg_code)

plastic_film_outputs <- use2012[plastic_film_code, ]
freshfruit_inputs <- use2012[which(use2012[,freshfruit_code] > 0), freshfruit_code, drop = FALSE]

# Increase demand of fresh fruit and vegetable industries for plastic film by some percent, decrease final demand of consumers for preserved fruits and vegetables by some percent so that we can see what the resulting net outcome is.

# Get cell values for fruit and veg's demand for plastic film.
plastic2fruitcell <- get_cells(use2012, plastic_film_code, freshfv_code)

# Make a loop for a number of possible increase values
plastic_demand_increase_vals <- seq(0, 100, 5)
use2012_increasedplasticdemand <- map(plastic_demand_increase_vals, ~ edit_cells(use2012, mutate(plastic2fruitcell, val = val*(1+./100))))

# Retotal the use tables
use2012_increasedplasticdemand <- map(use2012_increasedplasticdemand, retotal_usetable)

# 2. Write the edited use table to a csv.

iwalk(use2012_increasedplasticdemand, ~ write.csv(.x, file = file.path('~/Documents/temp', paste0('use2012plastic_', .y, '.csv'))))

# 3. Build USEEIO with the new table and write the resulting files to a directory.

source('~/Dropbox/projects/foodwaste/Code/USEEIO-master/R/Model Build Scripts/USEEIO2012_buildfunction.R')

model_names <- paste0('plasticscenario_', 1:length(plastic_demand_increase_vals))
use_table_files <- file.path('~/Documents/temp', paste0('use2012plastic_', 1:length(plastic_demand_increase_vals), '.csv'))

# Note: it takes a few seconds to build each model.
walk2(model_names, use_table_files, ~ build_USEEIO(outputfolder = file.path('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds', .x),
                                                  model = .x,
                                                  usetablefile = .y,
                                                  code_path = '~/Dropbox/projects/foodwaste/Code/USEEIO-master'))


# 4. Create a demand vector with the user's input.

# Load original demand vector.
all_codes <- read.csv('~/Dropbox/projects/foodwaste/Data/all_codes.csv', stringsAsFactors = FALSE)
demand2012 <- read.csv('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv', stringsAsFactors = FALSE, check.names = FALSE)

fruitveg_finaldemand_decrease_vals <- seq(0, 100, 5) 
# These decreases should be a percentage of the baseline amount of consumer level loss in LAFA, not including cooking loss.

# Here are lafa's numbers for that.
# Sum of fresh fruit and veg.
# total_preservedfruitveg_consumerwt <- 37.65 + 34.00 + 10.41 + 5.02
# total_preservedfruitveg_available <- 29.89 + 27.50 + 9.32 + 3.63
total_freshfruitveg_consumerwt <- 113.90 + 155.19
total_freshfruitveg_available <- 50.54 + 84.79
 
consumerloss_proportion <- 1 - (total_freshfruitveg_available/total_freshfruitveg_consumerwt) # Approx 50%.
# Note this is an extreme upper bound because much of the waste is "unavoidable."
# Later edit this.

orig_demand <- demand2012$`2012_US_Consumption`[match(freshfv_code, demand2012$BEA_389_code)]
demand_change_proportions <- 1 - consumerloss_proportion * fruitveg_finaldemand_decrease_vals/100
demand_values <- orig_demand %*% t(demand_change_proportions) 

apply(demand_values,2,sum) # Goes from 39B to around 24.6B

# Convert demand_values to a list of vectors
demand_values <- map(seq_len(ncol(demand_values)), ~ demand_values[,.x]) 

# 5. Run the python script from R to calculate the LCIA results (and possibly other results) from the built model and demand vector.

# Source python script for getting model results
source_python('~/Documents/GitHub/fwe/USEEIO/eeio_lcia.py')

# Outer loop: models; inner loop: demand values
# For now we are assembling the model with every iteration but if this is too slow we can certainly change that.
scenario_grid <- expand.grid(model_path = model_names, demand_values = demand_values, stringsAsFactors = FALSE)

# Ensure that every element of the demand_values list is itself a list
scenario_grid$demand_values <- map(scenario_grid$demand_values, as.list)

freshfv_drccode <- all_codes$sector_desc_drc[match(freshfv_code, all_codes$sector_code_uppercase)]

scenario_results <- pmap(scenario_grid, eeio_lcia, demand_codes = as.list(freshfv_drccode))


scenario_results <- pmap(scenario_grid, function(model_path, demand_values) eeio_lcia(model_path, as.list(demand_values), as.list(fruitveg_preservation_drccode)))

# 6. Combine output to single data frame.
demand_change_grid <- expand.grid(plastic_demand_increase = plastic_demand_increase_vals,
                                  fruitveg_demand_decrease = fruitveg_finaldemand_decrease_vals)
scenario_results_withID <- imap(scenario_results, ~ cbind(demand_change_grid[.y, ], impact_category = dimnames(.x)[[1]], .x))
scenario_results_withID <- do.call(rbind, scenario_results_withID)

# 7. Delete the intermediate files (edited use table and model build folder) unless user specifies not to delete.
invisible(file.remove(use_table_files))
unlink(file.path('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds', model_names), recursive = TRUE)

# 8. Write output to CSV.
write.csv(scenario_results_withID, 'Q:/scenario_results/plasticfilm_forfruitandveg.csv', row.names = FALSE)
