# Use tidyxl and unpivotr to load ERS FADS data and put into tidy form
# Only per capita availability tabs, not supply and use tabs
# QDR / FWE / 27 March 2019


# Load data ---------------------------------------------------------------


fp <- file.path(ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data'), 'ERS/FADS')

library(tidyverse)
library(tidyxl)
library(unpivotr)
library(zoo)

files <- dir(fp, pattern = 'xlsx', full.names = TRUE)
fruit_files <- files[grepl('fruit', files) & !grepl('veg', files)]
veg_files <- files[grepl('veg', files) & !grepl('tot|fruit', files)]
potato_files <- files[grepl('potato', files)]
fish_files <- files[grepl('fish', files)]
meat_files <- files[grepl('mtpcc', files)]

# Define "munging" functions ----------------------------------------------


# Function to get tidy cells for the PCC tabs for each sheet
get_pcc_tabs <- function(file, name_string = 'Pcc') {
  xlsx_cells(file) %>%
    rowwise %>%
    mutate(removed_sup = remove_superscripts(character_formatted),
           numeric = if_else(!is.na(as.numeric(removed_sup)), as.numeric(removed_sup), numeric),
           data_type = if_else(!is.na(as.numeric(removed_sup)), 'numeric', data_type)) %>% # Remove the superscripts on the data.
    ungroup %>%
    filter(grepl(name_string, sheet)) %>% # Get only Pcc total tabs
    group_by(sheet) %>% # Do all ops on each sheet separately
    mutate(is_data_row = row %in% (row[col == 1 & !is.na(numeric)])) %>% # Which rows have data in them
    filter(row <= max(row[is_data_row])) %>% # Get rid of rows below the data
    filter(col <= max(col[!is.na(numeric)])) %>% # Get rid of columns that go past the data
    select(sheet, row, col, data_type, numeric, character, is_data_row)
}

# Routine to remove the superscripts from some of the cells
remove_superscripts <- function(ch) {
  if (is_tibble(ch)) {
    return(ch$character[which(is.na(ch$vertAlign))[1]])
  } else {
    return(as.character(NA))
  }
}

# Function to tidy up one of the tabs
# Needs to specify whether there is a population column in the data frame, and how many grouping variables there are.
munge_tab <- function(dat) {
  has_population <- any(grepl('population', dat$character[dat$col == 2], ignore.case = TRUE))
  first_data_row <- min(dat$row[dat$is_data_row])
  #n_grouping_variables <- length(unique(dat$row[dat$row < first_data_row & !dat$data_type %in% 'blank']))
  
  # Create headers
  for (i in 1:(first_data_row - 1)) {
    dat <- dat %>% behead(direction = 'N', name = !!(paste0('group', i)))
  }
  
  # Create year header, and population header if appropriate
  dat <- dat %>% 
    behead('W', year)
  
  if (has_population) {
    dat <- dat %>%
      behead('W', population)
  }
  dat <- dat %>% 
    # The first grouping variable is not informative
    # Also all remaning rows should be data so we can remove those identifiers
    # Last group is a units identifier so rename it unit and get rid of the stupid hyphens around it.
    select(-group1, -is_data_row, -character, -data_type, -row, -col) %>%
    rename(variable_unit = !!as.name(paste0('group', first_data_row - 1))) %>%
    mutate(variable_unit = trimws(gsub('(^[-]+)|([-]+$)', '', variable_unit))) %>%
    # Get rid of columns with no information
    select_if(~!all(is.na(.))) %>%
    # Fill in NA values downward
    mutate(variable_unit = na.locf(variable_unit)) %>%
    mutate_at(vars(starts_with('group')), na.locf, na.rm = FALSE) %>%
    # Get rid of superscripts in labels
    mutate_at(vars(starts_with('group')), ~ trimws(gsub('[0-9]', '', .))) %>%
    rename(value = numeric)
  
  if ('group3' %in% names(dat)) {
    dat %>% rename(category = group2, food = group3)
  } else {
    dat %>% rename(food = group2)
  }
  
}

modified_munge <- function(dat) {
  has_population <- any(grepl('population', dat$character[dat$col == 2], ignore.case = TRUE))
  first_data_row <- min(dat$row[dat$is_data_row])
  #n_grouping_variables <- length(unique(dat$row[dat$row < first_data_row & !dat$data_type %in% 'blank']))
  
  # Create headers
  for (i in 1:(first_data_row - 1)) {
    dat <- dat %>% behead(direction = 'N', name = !!(paste0('group', i)))
  }
  
  # Create year header, and population header if appropriate
  dat <- dat %>% 
    behead('W', year)
  
  if (has_population) {
    dat <- dat %>%
      behead('W', population)
  }
  dat <- dat %>% 
    # The first grouping variable is not informative
    # Also all remaning rows should be data so we can remove those identifiers
    # Last group is a units identifier so rename it unit and get rid of the stupid hyphens around it.
    select(-group1, -is_data_row, -character, -data_type, -row, -col) %>%
    rename(variable_unit = !!as.name(paste0('group', first_data_row - 1))) %>%
    mutate(variable_unit = trimws(gsub('(^[-]+)|([-]+$)', '', variable_unit))) %>%
    # Get rid of columns with no information
    select_if(~!all(is.na(.))) %>%
    # Fill in NA values downward
    mutate(variable_unit = na.locf(variable_unit),
           group2 = trimws(na.locf(group2))) %>%
    mutate(group3 = if_else(group2 == 'Fresh market', group2, group3)) %>%
    mutate_at(vars(starts_with('group')), na.locf, na.rm = FALSE) %>%
    # Get rid of superscripts in labels
    mutate_at(vars(starts_with('group')), ~ trimws(gsub('[0-9]', '', .))) %>%
    rename(value = numeric)
  
  if ('group3' %in% names(dat)) {
    dat %>% rename(category = group2, food = group3)
  } else {
    dat %>% rename(food = group2)
  }
  
}


# Tidy the data! ----------------------------------------------------------


fruit_pcc <- map(fruit_files, get_pcc_tabs)
veg_pcc <- map(veg_files, get_pcc_tabs)
potato_pcc <- get_pcc_tabs(potato_files)
fish_pcc <- get_pcc_tabs(fish_files)
meat_pcc <- get_pcc_tabs(meat_files, name_string = 'Carcass|Retail|Boneless')

fruit_tidy <- map(fruit_pcc, ~ group_map(.x, ~ munge_tab(.x))) %>%
  map2(c('canned', 'dried', 'fresh', 'frozen', 'juice'), ~ data.frame(type = .y, .x, stringsAsFactors = FALSE)) %>%
  bind_rows %>%
  rename(availability_level = sheet) %>%
  select(type, availability_level, category, food, year, value, variable_unit)

veg_tidy <- map(veg_pcc, ~ group_map(.x, ~ munge_tab(.x))) %>%
  map2(c('canned', 'fresh', 'frozen'), ~ data.frame(type = .y, .x, stringsAsFactors = FALSE)) %>%
  bind_rows %>%
  rename(availability_level = sheet) %>%
  select(type, availability_level, food, year, value, variable_unit)

potato_tidy <- potato_pcc %>% 
  ungroup %>% 
  modified_munge %>%
  rename(availability_level = group4, supply_chain_stage = category, type = food) %>%
  select(availability_level, type, year, value, variable_unit)

fish_tidy <- fish_pcc %>%
  ungroup %>%
  munge_tab %>%
  rename(availability_level = group5, type = category) %>%
  select(type, availability_level, food, year, value, variable_unit) %>%
  mutate(food = if_else(food == 'Total', 'Total fish and shellfish', food))

meat_tidy <- meat_pcc %>%
  group_map(~ munge_tab(.x)) %>%
  rename(type = sheet) %>%
  mutate(food = if_else(category == 'Total', 'Total meat', food)) %>%
  mutate(food = if_else(category != 'Total' & food == 'Total', paste('Total', tolower(category)), food)) %>%
  select(type, food, year, value, variable_unit)

write.csv(fruit_tidy, file.path(fp, 'tidy_data/fruit_availability.csv'), row.names = FALSE)
write.csv(veg_tidy, file.path(fp, 'tidy_data/veg_availability.csv'), row.names = FALSE)
write.csv(potato_tidy, file.path(fp, 'tidy_data/potato_availability.csv'), row.names = FALSE)
write.csv(fish_tidy, file.path(fp, 'tidy_data/fish_availability.csv'), row.names = FALSE)
write.csv(meat_tidy, file.path(fp, 'tidy_data/meat_availability.csv'), row.names = FALSE)
