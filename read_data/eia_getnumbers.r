# Get totals from EIA data
# QDR FWE 29 Jan 2019

library(tidyverse)

fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')

fuel <- read.csv(file.path(fp, 'EIA/processed/fuel_used_usa.csv'), stringsAsFactors = FALSE)
appl <- read.csv(file.path(fp, 'EIA/processed/appliances_usa.csv'), stringsAsFactors = FALSE)
heat <- read.csv(file.path(fp, 'EIA/processed/waterheating_usa.csv'), stringsAsFactors = FALSE)
