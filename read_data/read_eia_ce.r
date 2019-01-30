# Read CE data sets from EIA
# QDR FWE 29 Jan 2019


# Define function to read EIA data ----------------------------------------

read_eia <- function(filename) {
  require(tidyxl)
  require(dplyr)
  require(purrr)
  require(tidyr)
  
  # Read XLSX file
  sheet_data <- xlsx_cells(filename, sheets = c('data', 'rse'), include_blank_cells = FALSE)
  sheet_format <- xlsx_formats(filename)
  
  # Find indented and bold format IDs
  indented_cells <- which(sheet_format$local$alignment$indent > 0)
  bold_cells <- which(sheet_format$local$font$bold)
  
  # Find rows containing the numeric values
  num_rows <- with(sheet_data, unique(row[data_type %in% 'numeric' | character %in% c('Q','N')]))
  
  # Find rows with headers or groups of headers
  header_rows <- with(sheet_data, unique)
  
  # Label cells with their formats
  sheet_data <- sheet_data %>%
    mutate(is_indented = local_format_id %in% indented_cells,
           is_bold = local_format_id %in% bold_cells,
           is_primary_heading = is_bold & row >= min(num_rows),
           is_headerrow = row == min(num_rows) - 1,
           is_data_cell = (data_type %in% 'numeric' | character %in% c('Q','N')) & col > 1)
  sheet_data <- sheet_data %>%
    mutate(character = if_else(col == 1 & is.na(character), as.character(numeric), character))
  
  # Get grouping variables that go with each data cell and clean the text
  sheet_verylong <- sheet_data %>%
    filter(is_data_cell) %>%
    pmap_dfr(get_vars_for_data_cell, parent_sheet = sheet_data) %>%
    mutate(group3 = if_else(grepl('Release date', group3), as.character(NA), group3)) %>%
    mutate_at(vars(group1:group4), function(x) if_else(nchar(x) > 2, gsub('[[:digit:]]$', '', x), as.character(x))) %>%
    mutate_at(vars(group1:group4), function(x) gsub('\r', '', x, fixed = TRUE)) %>%
    mutate_at(vars(group1:group4), function(x) gsub('\n', '', x, fixed = TRUE)) %>%
    mutate_at(vars(group1:group4), function(x) gsub(' (more than one may apply)', '', x, fixed = TRUE))
  
  # Reshape and return
  sheet_verylong %>%
    group_by(group1, group2, group3, group4) %>%
    spread(value_type, value)
}

# Function to get all grouping variables for a given cell
get_vars_for_data_cell <- function(parent_sheet, sheet, row, col, numeric, character, ...) {
  bold_rows <- unique(parent_sheet$row[parent_sheet$is_bold])
  bold_rows <- bold_rows[bold_rows <= row]
  g1idx <- which(parent_sheet$sheet == sheet & parent_sheet$col == col & parent_sheet$is_headerrow)
  g2idx <- which(parent_sheet$sheet == sheet & parent_sheet$is_bold & parent_sheet$row == max(bold_rows) & parent_sheet$col == 1)
  g3idx <- which(parent_sheet$sheet == sheet & !parent_sheet$is_bold & !parent_sheet$is_indented & parent_sheet$row <= row & parent_sheet$col == 1)
  g4idx <- which(parent_sheet$sheet == sheet & parent_sheet$row == row & parent_sheet$is_indented & parent_sheet$col == 1)
  data.frame(value_type = sheet, 
             value = numeric, 
             flag = character, 
             group1 = ifelse(length(g1idx) == 1, parent_sheet$character[g1idx], NA),
             group2 = ifelse(length(g2idx) == 1, parent_sheet$character[g2idx], NA),
             group3 = parent_sheet$character[max(g3idx, na.rm = TRUE)],
             group4 = ifelse(length(g4idx) == 1, parent_sheet$character[g4idx], NA),
             stringsAsFactors = FALSE)
}


# Read EIA data -----------------------------------------------------------

fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')

ce11 <- read_eia(file.path(fp, 'EIA/ce1.1.xlsx'))
