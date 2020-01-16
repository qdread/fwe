# Read CE data sets from EIA
# QDR FWE 29 Jan 2019


# Define function to read EIA data ----------------------------------------

read_eia <- function(filename, sheets_to_read = c('data', 'rse')) {
  require(tidyxl)
  require(dplyr)
  require(purrr)
  require(tidyr)
  require(zoo)
  
  # Read XLSX file
  sheet_data <- xlsx_cells(filename, sheets = sheets_to_read, include_blank_cells = TRUE)
  sheet_format <- xlsx_formats(filename)
  
  # Find indented and bold format IDs
  indented_cells <- which(sheet_format$local$alignment$indent > 0)
  bold_cells <- which(sheet_format$local$font$bold)
  
  # Find rows containing the numeric values
  num_rows <- with(sheet_data, unique(row[data_type %in% 'numeric' | character %in% c('Q','N')]))
  
  # Find rows with headers or groups of headers
  bold_rows <- with(sheet_data, unique(row[local_format_id %in% bold_cells]))
  header_rows <- bold_rows[bold_rows < min(num_rows) & bold_rows > 2]
  
  # First header row is units
  unit_row <- header_rows[1]
  header_rows <- header_rows[-1]

  # Fill merged cells in the header row group with cells either ahead or behind them
  sheet_data <- sheet_data %>%
    group_by(sheet, row) %>%
    mutate(character = if_else(row %in% c(unit_row, header_rows), na.locf(character, na.rm = FALSE), character)) %>%
    ungroup
  
  # Label cells with their formats
  sheet_data <- sheet_data %>%
    mutate(is_indented = local_format_id %in% indented_cells,
           is_bold = local_format_id %in% bold_cells,
           is_grouping_variable = is_bold & row >= min(num_rows),
           is_unitrow = row == unit_row,
           is_headerrow = row %in% header_rows,
           is_data_cell = (data_type %in% 'numeric' | character %in% c('Q','N')) & col > 1)
  sheet_data <- sheet_data %>%
    mutate(character = if_else(col == 1 & is.na(character), as.character(numeric), character))
  
  # Replace unit row in RSE sheet with corresponding unit row in data sheet, since they do not match
  sheet_data$character[(sheet_data$is_unitrow | sheet_data$is_headerrow) & sheet_data$sheet == 'rse'] <- sheet_data$character[(sheet_data$is_unitrow | sheet_data$is_headerrow) & sheet_data$sheet == sheets_to_read[1]]
  
  # Get grouping variables that go with each data cell and clean the text
  sheet_verylong <- sheet_data %>%
    filter(is_data_cell) %>%
    pmap_dfr(get_vars_for_data_cell, parent_sheet = sheet_data) %>%
    mutate(group3 = if_else(grepl('Release date', group3), as.character(NA), group3)) %>%
    mutate_at(vars(matches('unit|header|group')), function(x) {
        x <- if_else(nchar(x) > 2, gsub('[[:digit:]]$', '', x), as.character(x))
        x <- gsub('\r', '', x, fixed = TRUE)
        x <- gsub('\n', '', x, fixed = TRUE)
        gsub(' (more than one may apply)', '', x, fixed = TRUE)
      })

  # Reshape and return
  sheet_verylong %>%
    group_by_at(vars(matches('unit|header|group'))) %>%
    spread(value_type, value)
}

# Function to get all grouping variables for a given cell
get_vars_for_data_cell <- function(parent_sheet, sheet, row, col, numeric, character, ...) {
  bold_rows <- unique(parent_sheet$row[parent_sheet$is_bold])
  bold_rows <- bold_rows[bold_rows <= row]
  unitidx <- which(parent_sheet$sheet == sheet & parent_sheet$col == col & parent_sheet$is_unitrow)
  headeridx <- which(parent_sheet$sheet == sheet & parent_sheet$col == col & parent_sheet$is_headerrow)
  header_vec <- parent_sheet$character[headeridx]
  header_vec <- c(na.omit(header_vec), rep('none', sum(is.na(header_vec))))
  header_df <- setNames(as.data.frame(t(header_vec), stringsAsFactors = FALSE), paste0('header', 1:length(header_vec)))
  g1idx <- which(parent_sheet$sheet == sheet & parent_sheet$is_bold & parent_sheet$row == max(bold_rows) & parent_sheet$col == 1)
  g2idx <- which(parent_sheet$sheet == sheet & !parent_sheet$is_bold & !parent_sheet$is_indented & parent_sheet$row <= row & parent_sheet$col == 1 & parent_sheet$row > 1)
  g3idx <- which(parent_sheet$sheet == sheet & parent_sheet$row == row & parent_sheet$is_indented & parent_sheet$col == 1)
  data.frame(value_type = sheet, 
             value = numeric, 
             flag = character, 
             unit = ifelse(length(unitidx) == 1, parent_sheet$character[unitidx], NA),
             header_df,
             group1 = ifelse(length(g1idx) == 1, parent_sheet$character[g1idx], NA),
             group2 = ifelse(length(g2idx) >= 1, parent_sheet$character[max(g2idx, na.rm = TRUE)], NA),
             group3 = ifelse(length(g3idx) == 1, parent_sheet$character[g3idx], NA),
             stringsAsFactors = FALSE)
}


# Read EIA data -----------------------------------------------------------

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')

# Summary stats on consumptions and expenditures 1.1
ce11 <- read_eia(file.path(fp, 'EIA/ce1.1.xlsx'))

# Fuel consumption by fuel 2.1
ce21 <- read_eia(file.path(fp, 'EIA/ce2.1.xlsx'), c('Btu', 'rse'))

# Fuel expenditure by fuel 2.6
ce26 <- read_eia(file.path(fp, 'EIA/ce2.6.xlsx'), c('dollars', 'rse'))

# Consumption by end use 3.1
ce31 <- read_eia(file.path(fp, 'EIA/ce3.1.xlsx'), c('Btu', 'rse'))

# Expenditure by end use 3.6
ce36 <- read_eia(file.path(fp, 'EIA/ce3.6.xlsx'), c('dollars', 'rse'))

# Fuel consumption by fuel and end use (totals) 4.1
ce41 <- read_eia(file.path(fp, 'EIA/ce4.1.xlsx'), c('Btu', 'rse'))

# Fuel expenditure by fuel and end use (totals) 4.11
ce411 <- read_eia(file.path(fp, 'EIA/ce4.11.xlsx'), c('dollars', 'rse'))

# Detailed consumption for electricity by end use, totals. 2 parts, 5.1a and 5.1b
ce51a <- read_eia(file.path(fp, 'EIA/ce5.1a.xlsx'), c('kWh', 'rse'))
ce51b <- read_eia(file.path(fp, 'EIA/ce5.1b.xlsx'), c('kWh', 'rse'))
ce51 <- rbind(ce51a, ce51b)

# Detailed consumption for natural gas and propane by end use, totals 5.2
ce52 <- read_eia(file.path(fp, 'EIA/ce5.2edited.xlsx'), c('Btu', 'rse'))

# Detailed expenditure for electricity by end use, totals. 2 parts, 5.5a and 5.5b
ce55a <- read_eia(file.path(fp, 'EIA/ce5.5a.xlsx'), c('dollars', 'rse'))
ce55b <- read_eia(file.path(fp, 'EIA/ce5.5b.xlsx'), c('dollars', 'rse'))
ce55 <- rbind(ce55a, ce55b)

# Detailed expenditure for natural gas and propane by end use, totals 5.6
ce56 <- read_eia(file.path(fp, 'EIA/ce5.6.xlsx'), c('dollars', 'rse'))


# Save as CSVs ------------------------------------------------------------

# Save only the ones in dollar amounts because they are more comparable.
fpsave <- file.path(fp, 'EIA/processed')

write.csv(ce11, file.path(fpsave, 'EIA_consumption_expenditure_summarystats.csv'), row.names = FALSE)
write.csv(ce26, file.path(fpsave, 'EIA_expenditure_byfuel.csv'), row.names = FALSE)
write.csv(ce36, file.path(fpsave, 'EIA_expenditure_byuse.csv'), row.names = FALSE)
write.csv(ce411, file.path(fpsave, 'EIA_expenditure_byfuelXuse.csv'), row.names = FALSE)
write.csv(ce55, file.path(fpsave, 'EIA_expenditure_detailed_electricity.csv'), row.names = FALSE)
write.csv(ce56, file.path(fpsave, 'EIA_expenditure_detailed_naturalgas.csv'), row.names = FALSE)
