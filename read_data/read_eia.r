# Read EIA data
# QDR FWE 16 Jan 2019

library(XLConnect)
library(dplyr)

fp <- ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data')


eia_list <- lapply(1:10, function(i) {
  
})

# HC1: fuels used and end uses
hc1.1 <- readWorksheetFromFile(file.path(fp, 'EIA/hc1.1.xlsx'), sheet = 'data')
hc2.1 <- readWorksheetFromFile(file.path(fp, 'EIA/hc2.1.xlsx'), sheet = 'data')
hc1.2 <- readWorksheetFromFile(file.path(fp, 'EIA/hc1.2.xlsx'), sheet = 'data')

num_rows <- apply(hc1.1, 1, function(x) any(!is.na(as.numeric(x))) | any(x %in% c('Q', 'N')))
hc1.1_numeric <- hc1.1[num_rows,]
hc1.1[!num_rows, 1]

# Make the first level heading and second level heading into two columns
heading1 <- hc1.1[which(!num_rows)[which(!num_rows) > min(which(num_rows))], 1]
rl <- diff(which(!num_rows)[which(!num_rows) > min(which(num_rows))]) - 1
rl <- rl[rl > 0]

hc1.1_fixed <- cbind(variable = c('All homes', rep(heading1[1:length(rl)], times=rl)), hc1.1_numeric)
names(hc1.1_fixed)[2] <- 'type'
colheaders <- hc1.1[min(which(num_rows)) - 1, ]

# Get column names 
col_names <- hc1.1[min(which(num_rows)) - 1, -1]
names(hc1.1_fixed)[3:ncol(hc1.1_fixed)] <- col_names

# Remove "more than one may apply" and the numbers from variables and headers (only the numbers at the end)
names(hc1.1_fixed) <- gsub('[[:digit:]]$', '', names(hc1.1_fixed))
names(hc1.1_fixed) <- gsub('\n', '', names(hc1.1_fixed), fixed = TRUE)
hc1.1_fixed <- hc1.1_fixed %>%
  mutate(type = gsub('[[:digit:]]$', '', type),
         variable = gsub(' (more than one may apply)', '', variable, fixed = TRUE))

# Get rid of the "total" row and then turn to long data
hc1.1_fixed %>%
  filter(!variable %in% 'All homes') %>%
  

# Convert the bad quality flags to NAs and make a flag column, then convert the remaining values to numerics.
  
# See if function works on any other data frames.




# tidyxl workflow to read -------------------------------------------------

hc1.1 <- xlsx_cells(file.path(fp, 'EIA/hc1.1.xlsx'), sheets = c('data', 'rse'), include_blank_cells = FALSE)
hc1.1_format <- xlsx_formats(file.path(fp, 'EIA/hc1.1.xlsx'))
indented_cells <- which(hc1.1_format$local$alignment$indent > 0)
bold_cells <- which(hc1.1_format$local$font$bold)
num_rows <- with(hc1.1, unique(row[data_type %in% 'numeric' | character %in% c('Q','N')]))
num_cols <- with(hc1.1, unique(col[data_type %in% 'numeric' | character %in% c('Q','N')]))

# function to get blocks of consecutive numbers
get_blocks <- function(Vec) {
  Breaks <- c(0, which(diff(Vec) != 1), length(Vec))
  block_list <- sapply(seq(length(Breaks) - 1),
         function(i) Vec[(Breaks[i] + 1):Breaks[i+1]]) 
  rep(1:length(block_list), sapply(block_list, length))
}

num_blocks <- get_blocks(num_rows)

hc1.1 <- hc1.1 %>%
  left_join(data.frame(row = num_rows, num_block = num_blocks)) %>%
  mutate(is_indented = local_format_id %in% indented_cells,
         is_bold = local_format_id %in% bold_cells,
         is_primary_heading = is_bold & row >= min(num_rows),
         is_headerrow = row == min(num_rows) - 1,
         is_data_cell = data_type %in% 'numeric' | character %in% c('Q','N'))

# Reshape the data frame using multiple groups
# Function to find the 4 grouping variables to go with each value cell
library(purrr)
library(tidyr)

hc1.1_verylong <- hc1.1 %>% 
  filter(is_data_cell) %>%
  pmap_dfr(function(sheet, row, col, numeric, character, ...) {
    bold_rows <- unique(hc1.1$row[hc1.1$is_bold])
    bold_rows <- bold_rows[bold_rows <= row]
    g1idx <- which(hc1.1$sheet == sheet & hc1.1$col == col & hc1.1$is_headerrow)
    g2idx <- which(hc1.1$sheet == sheet & hc1.1$is_bold & hc1.1$row == max(bold_rows) & hc1.1$col == 1)
    g3idx <- which(hc1.1$sheet == sheet & !hc1.1$is_bold & !hc1.1$is_indented & hc1.1$row <= row & hc1.1$col == 1)
    g4idx <- which(hc1.1$sheet == sheet & hc1.1$row == row & hc1.1$is_indented & hc1.1$col == 1)
    data.frame(value_type = sheet, 
               value = numeric, 
               flag = character, 
               group1 = hc1.1$character[g1idx],
               group2 = hc1.1$character[g2idx],
               group3 = hc1.1$character[max(g3idx)],
               group4 = ifelse(length(g4idx) == 1, hc1.1$character[g4idx], NA),
               stringsAsFactors = FALSE)
  })


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

hc1.1_verylong <- hc1.1 %>%
  filter(is_data_cell) %>%
  pmap_dfr(get_vars_for_data_cell, parent_sheet = hc1.1) %>%
  mutate(group3 = if_else(grepl('Release date', group3), as.character(NA), group3)) %>%
  mutate_at(vars(group1:group4), function(x) gsub('[[:digit:]]$', '', x)) %>%
  mutate_at(vars(group1:group4), function(x) gsub('\r', '', x, fixed = TRUE)) %>%
  mutate_at(vars(group1:group4), function(x) gsub('\n', '', x, fixed = TRUE)) %>%
  mutate_at(vars(group1:group4), function(x) gsub(' (more than one may apply)', '', x, fixed = TRUE))

hc1.1_reshaped <- hc1.1_verylong %>%
  group_by(group1, group2, group3, group4) %>%
  spread(value_type, value)


read_eia <- function(filename) {
  require(tidyxl)
  require(dplyr)
  require(purrr)
  require(tidyr)
  sheet_data <- xlsx_cells(filename, sheets = c('data', 'rse'), include_blank_cells = FALSE)
  sheet_format <- xlsx_formats(filename)
  indented_cells <- which(sheet_format$local$alignment$indent > 0)
  bold_cells <- which(sheet_format$local$font$bold)
  num_rows <- with(sheet_data, unique(row[data_type %in% 'numeric' | character %in% c('Q','N')]))
  sheet_data <- sheet_data %>%
    mutate(is_indented = local_format_id %in% indented_cells,
           is_bold = local_format_id %in% bold_cells,
           is_primary_heading = is_bold & row >= min(num_rows),
           is_headerrow = row == min(num_rows) - 1,
           is_data_cell = (data_type %in% 'numeric' | character %in% c('Q','N')) & col > 1)
  sheet_data <- sheet_data %>%
    mutate(character = if_else(col == 1 & is.na(character), as.character(numeric), character))
  sheet_verylong <- sheet_data %>%
    filter(is_data_cell) %>%
    pmap_dfr(get_vars_for_data_cell, parent_sheet = sheet_data) %>%
    mutate(group3 = if_else(grepl('Release date', group3), as.character(NA), group3)) %>%
    mutate_at(vars(group1:group4), function(x) if_else(nchar(x) > 2, gsub('[[:digit:]]$', '', x), as.character(x))) %>%
    mutate_at(vars(group1:group4), function(x) gsub('\r', '', x, fixed = TRUE)) %>%
    mutate_at(vars(group1:group4), function(x) gsub('\n', '', x, fixed = TRUE)) %>%
    mutate_at(vars(group1:group4), function(x) gsub(' (more than one may apply)', '', x, fixed = TRUE))
  
  sheet_verylong %>%
    group_by(group1, group2, group3, group4) %>%
    spread(value_type, value)
}

fuel_used_northeast_midwest <- read_eia(file.path(fp, 'EIA/hc1.7.xlsx'))
fuel_used_south_west <- read_eia(file.path(fp, 'EIA/hc1.8.xlsx'))

fuel_used_usa <- read_eia(file.path(fp, 'EIA/hc1.1.xlsx'))
appliances_usa <- read_eia(file.path(fp, 'EIA/hc3.1edited.xlsx'))
waterheating_usa <- read_eia(file.path(fp, 'EIA/hc8.1.xlsx'))

# Save data
write.csv(fuel_used_usa, file = file.path(fp, 'EIA/processed/fuel_used_usa.csv'), row.names = FALSE)
write.csv(appliances_usa, file = file.path(fp, 'EIA/processed/appliances_usa.csv'), row.names = FALSE)
write.csv(waterheating_usa, file = file.path(fp, 'EIA/processed/waterheating_usa.csv'), row.names = FALSE)
