# Use tidyxl and unpivotr to load ERS FADS data and put into tidy form
# QDR / FWE / 27 March 2019

fp <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'ERS/FADS')

library(tidyverse)
library(tidyxl)
library(unpivotr)

files <- dir(fp, pattern = 'xlsx', full.names = TRUE)

# test
ctcspraw <- xlsx_cells(files[1])
