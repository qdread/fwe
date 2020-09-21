# Script for reading LAFA XLSX files into R
# QDR/FWE/01 Oct 2018

# Updated 21 Sep 2020 to use readxl
# Updated 02 Nov 2018 to run both remotely and locally

# Define functions --------------------------------------------------------

# Function to read single sheet
read_lafa_sheet <- function(name, file) {
  sheet_raw <- suppressMessages(read_xls(file, sheet = name))
  # Find the index of the first and last row that contains a year number in the first column.
  numeric_col1 <- as.numeric(str_extract_all(pull(sheet_raw, 1), '^[0-9]{4}', simplify = TRUE))
  year_idx <- c(which.min(numeric_col1), which.max(numeric_col1))
  # Read the sheet again with only those rows.
  sheet_dat <- suppressMessages(
    read_xls(file, 
             sheet = name, 
             skip = year_idx[1],
             n_max = diff(year_idx) + 1,
             col_names = FALSE,
             col_types = 'numeric'))
  
  # Get the row with units in it.
  unit_row <- suppressMessages(read_xls(file, sheet = name, skip = year_idx[1] - 1, n_max = 1, col_names = FALSE))
  # Parse it
  unit_row_parsed <- trimws(gsub('-', '', as.character(unit_row)))
  unit_row_parsed <- gsub('/', '.', unit_row_parsed)
  # Get rid of any characters that are not alphanumeric at the end of the line.
  unit_row_parsed <- gsub('[^a-zA-Z0-9]*$', '', unit_row_parsed)
  
  # Read in header rows (row1 always has a title)
  header_rows <- suppressMessages(read_xls(file, sheet = name, skip = 1, n_max = 2, col_names = FALSE))
  # Fill first header row forward if there is a NA
  header_row1 <- na.locf(unlist(header_rows[1,]))
  # Paste second header row onto first, if it exists
  header_row2 <- unlist(header_rows[2,])
  header_row_parsed <- if_else(is.na(header_row2), header_row1, paste(header_row1, header_row2, sep = '_'))
  header_row_parsed <- gsub('[0-9]', '', header_row_parsed)
  header_row_parsed <- gsub('[^a-zA-Z\\s]', '_', header_row_parsed)
  
  
  # Get rid of excess header rows for cells present by mistake
  header_row_parsed <- header_row_parsed[2:(length(unit_row_parsed) + 1)]

  setNames(sheet_dat, c('Year', paste(header_row_parsed, unit_row_parsed, sep = '_')))
}

# Function to read entire workbook
read_lafa_workbook <- function(file) {
  require(tidyverse)
  require(readxl)
  require(zoo)
  
    # Get sheet names and remove table of contents
  sheet_names <- excel_sheets(file)
  sheet_names <- sheet_names[!sheet_names %in% c('TableOfContents')]
  
  all_sheets <- map_dfr(sheet_names, ~ data.frame(Category = ., read_lafa_sheet(., file = file), stringsAsFactors = FALSE))
  
  return(all_sheets)
}


# Read data ---------------------------------------------------------------

fp_lafa <- ifelse(dir.exists('Q:/'), 'Q:/raw_data/USDA/LAFA', ifelse(dir.exists('/nfs/qread-data'), '/nfs/qread-data/raw_data/USDA/LAFA', '~/Dropbox/Q/projects/foodwaste/Data/LAFA_localcopy'))

dairy <- read_lafa_workbook(file.path(fp_lafa, 'Dairy.xls'))
fat <- read_lafa_workbook(file.path(fp_lafa, 'fat.xls'))
fruit <- read_lafa_workbook(file.path(fp_lafa, 'Fruit.xls'))
grain <- read_lafa_workbook(file.path(fp_lafa, 'grain.xls'))
meat <- read_lafa_workbook(file.path(fp_lafa, 'meat.xls'))
sugar <- read_lafa_workbook(file.path(fp_lafa, 'sugar.xls'))
veg <- read_lafa_workbook(file.path(fp_lafa, 'veg.xls'))

