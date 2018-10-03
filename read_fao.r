# Script for reading FAOSTAT CSV files into R
# QDR/FWE/02 Oct 2018


# Read in CSVs ------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(data.table)

files <- list.files('Q:/FAOSTAT', full.names = TRUE)
objnames <- gsub('(.*?)usa_', '', files)
objnames <- gsub('.csv', '', objnames)

walk2(files, objnames, function(f, n) assign(n, read.csv(f, stringsAsFactors = FALSE), envir = .GlobalEnv))


# Tidy production and FBS data --------------------------------------------

# Convenience function to replace non alphanumeric with underscore
underscore <- function(x) gsub('[^a-zA-Z0-9\\s]', '_', x)

# Function to tidy production data frame.
tidy_production <- function(dat) {
  dat %>%
    mutate(Element = paste(underscore(Element), underscore(Unit), sep = '_')) %>%
    select(Element, Item, Year, Flag, Value) %>%
    setDT %>%
    dcast(Item + Year ~ Element, value.var = c('Value', 'Flag')) 
}

productionqty_crops_tidy <- tidy_production(productionqty_crops)
productionqty_cropsprocessed_tidy <- tidy_production(productionqty_cropsprocessed)  
productionqty_livestockprimary_tidy <- tidy_production(productionqty_livestockprimary)  
productionqty_livestockprocessed_tidy <- tidy_production(productionqty_livestockprocessed)  
producerpriceannual_tidy <- tidy_production(producerpriceannual)
fbs_tidy <- tidy_production(fbs)
croptrade_tidy <- tidy_production(croptrade)
