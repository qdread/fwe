# Use tidyxl and unpivotr to load ERS FADS data and put into tidy form
# QDR / FWE / 27 March 2019

fp <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'ERS/FADS')

library(tidyverse)
library(tidyxl)
library(unpivotr)
library(zoo)

files <- dir(fp, pattern = 'xlsx', full.names = TRUE)

# test
freshfruit_raw <- xlsx_cells(files[8])

# Let the munging begin
x <- freshfruit_raw

x <- x %>%
  filter(!grepl('Contents', sheet)) %>% # Remove sheet with table of contents
  group_by(sheet) # Do all ops on each sheet separately
  mutate(is_data_row = row %in% (row[col == 1 & !is.na(numeric)])) %>% # Which rows have data in them
  filter(row <= max(row[is_data_row])) %>% # Get rid of rows below the data
  filter(col <= max(col[!is.na(numeric)])) %>% # Get rid of columns that go past the data
  select(sheet, row, col, data_type, numeric, character, is_data_row)

munge_one_tab <- function(dat) {
  # How many rows are header rows
  n_header_rows <- with(dat, min(row[is_data_row] - 1))
  
  for (i in 1:n_header_rows) {
    dat <- dat %>% behead(direction = 'N', name = !!(paste0('group', i)))
  }
  
  # The first two columns are year and population which are grouping variables
  dat <- dat %>% 
    behead('W', year) %>% 
    behead('W', population) %>%
  # The first grouping variable is not informative
  # Also all remaning rows should be data so we can remove those identifiers
  # Last group is a units identifier so rename it unit and get rid of the stupid hyphens around it.
    select(-group1, -is_data_row, -character) %>%
    rename(variable_unit = !!as.name(paste0('group', n_header_rows))) %>%
    mutate(variable_unit = trimws(gsub('(^[-]+)|([-]+$)', '', variable_unit))) %>%
  # Fill in the NA areas in the grouping variables
  # Units can carry forward forever
  # group6 has no useful info
  # group2 and group3 should combine together
    mutate(group3 = coalesce(group3, group4),
           group2 = na.locf(group2),
           group3 = na.locf(group3),
           variable_unit = na.locf(variable_unit)) %>%
    select(-group4, -group6) %>%
    mutate_at(vars(starts_with('group')), ~ gsub('[0-9]', '', .)) %>%
    rename(category = group2,
           subcategory = group3,
           availability_level = group5,
           value = numeric) %>%
    select(year, population, category, subcategory, availability_level, variable_unit, value)
}  

x %>% 
  group_map(~ munge_one_tab(.x))

# test code below here ----------------------------------------------------


# Try to get grapefruit into data form
grapefruit <- x %>% ungroup %>% filter(sheet=='Grapefruit')
# How many rows are header rows
n_header_rows <- with(grapefruit, min(row[is_data_row] - 1))

for (i in 1:n_header_rows) {
  grapefruit <- grapefruit %>% behead(direction = 'N', name = !!(paste0('group', i)))
}

# The first two columns are year and population which are grouping variables
grapefruit <- grapefruit %>% behead('W', Year)
grapefruit <- grapefruit %>% behead('W', Population)

# The first grouping variable is not informative
# Also all remaning rows should be data so we can remove those identifiers
grapefruit <- grapefruit %>% select(-group1, -is_data_row, -character)

# Last group is a units identifier so rename it unit and get rid of the stupid hyphens around it.
grapefruit <- grapefruit %>% rename(variable_unit = !!as.name(paste0('group', n_header_rows))) %>%
  mutate(variable_unit = trimws(gsub('(^[-]+)|([-]+$)', '', variable_unit)))

# Fill in the NA areas in the grouping variables
# Units can carry forward forever
# group6 is crap
# group2 and group3 should combine together

grapefruit %>%
  mutate(group3 = coalesce(group3, group4),
         group2 = na.locf(group2),
         group3 = na.locf(group3),
         variable_unit = na.locf(variable_unit)) %>%
  select(-group4, -group6) %>%
  mutate_at(vars(starts_with('group')), ~ gsub('[0-9]', '', .)) %>%
  rename(category = group2,
         subcategory = group3,
         availability_level = group5,
         value = numeric) %>%
  select(Year, Population, category, subcategory, availability_level, variable_unit, value)
