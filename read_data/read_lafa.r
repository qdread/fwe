# Script for reading LAFA XLSX files into R
# QDR/FWE/01 Oct 2018

# Updated 02 Nov 2018 to run both remotely and locally

# Define functions --------------------------------------------------------

# Function to read single sheet
read_lafa_sheet <- function(name, wb) {
  sheet_raw <- readWorksheet(wb, sheet = name)
  # Find the index of the first and last row that contains a year number in the first column.
  numeric_col1 <- as.numeric(str_extract_all(sheet_raw[,1], '^[0-9]{4}', simplify = TRUE))
  year_idx <- c(which.min(numeric_col1), which.max(numeric_col1))
  # Read the sheet again with only those rows.
  sheet_dat <- readWorksheet(wb, 
                             sheet = name, 
                             startRow = year_idx[1] + 1, 
                             endRow = year_idx[2] + 1, 
                             header = FALSE,
                             colTypes = 'numeric',
                             forceConversion = TRUE)
  # Get the row with units in it.
  unit_row <- readWorksheet(wb, sheet = name, startRow = year_idx[1], endRow = year_idx[1], header = FALSE)
  # Parse it
  unit_row_parsed <- trimws(gsub('-', '', as.character(unit_row)))
  unit_row_parsed <- gsub('/', '.', unit_row_parsed)
  # Get rid of any characters that are not alphanumeric at the end of the line.
  unit_row_parsed <- gsub('[^a-zA-Z0-9]*$', '', unit_row_parsed)
  
  # Read in header rows (row1 always has a title)
  header_rows <- readWorksheet(wb, sheet = name, startRow = 2, endRow = 3, header = FALSE)
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
  require(XLConnect)
  require(stringr)
  require(zoo)
  require(purrr)
  require(dplyr)
  
  wb <- loadWorkbook(file, create = TRUE)
  
  # Get sheet names and remove table of contents
  sheet_names <- getSheets(wb)
  sheet_names <- sheet_names[!sheet_names %in% c('TableOfContents')]
  
  all_sheets <- map(sheet_names, read_lafa_sheet, wb = wb)
  all_sheets <- map2(sheet_names, all_sheets, function(x, y) data.frame(Category = x, y, stringsAsFactors = FALSE))
  
  bind_rows(all_sheets)
}


# Read data ---------------------------------------------------------------

if (dir.exists('Z:/')) fp <- 'Z:/ERS/LAFA' else fp <- '~/Dropbox/projects/foodwaste/Data/LAFA_localcopy'

dairy <- read_lafa_workbook(file.path(fp, 'Dairy.xls'))
fat <- read_lafa_workbook(file.path(fp, 'fat.xls'))
fruit <- read_lafa_workbook(file.path(fp, 'Fruit.xls'))
grain <- read_lafa_workbook(file.path(fp, 'grain.xls'))
meat <- read_lafa_workbook(file.path(fp, 'meat.xls'))
sugar <- read_lafa_workbook(file.path(fp, 'sugar.xls'))
veg <- read_lafa_workbook(file.path(fp, 'veg.xls'))

