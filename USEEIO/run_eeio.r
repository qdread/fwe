# Function to take an edited use table and demand vector, build the USEEIO model from it, get the results out, and delete the resulting model
# QDR / FWE / 07 Dec 2018

run_eeio <- function(model_name, use_table_file, demand_values, demand_codes) {
  
  require(dplyr)
  require(reticulate)

  # Build model with use table file
  source('~/Dropbox/projects/foodwaste/Code/USEEIO-master/R/Model Build Scripts/USEEIO2012_buildfunction.R')
  
  output_folder <- file.path('~/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds', model_name)
  
  build_USEEIO(outputfolder = output_folder,
               model = model_name,
               usetablefile = use_table_file,
               code_path = '~/Dropbox/projects/foodwaste/Code/USEEIO-master')
  
  # Source python script for getting model results
  source_python('~/Documents/GitHub/fwe/USEEIO/eeio_lcia.py')
  
  # Get model results with demand dictionary
  lcia <- eeio_lcia(model_path = model_name, demand_values = demand_values, demand_codes = demand_codes)
  
  # Delete model files
  unlink(output_folder, recursive = TRUE)
  
  return(lcia)
  
}
