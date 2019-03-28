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

# Routine to remove the superscripts from some of the cells
remove_superscripts <- function(ch) {
  if (is_tibble(ch)) {
    return(ch$character[which(is.na(ch$vertAlign))[1]])
  } else {
    return(as.character(NA))
  }
}

freshfruit_raw <- freshfruit_raw %>%
  rowwise %>%
  mutate(removed_sup = remove_superscripts(character_formatted),
         numeric = if_else(!is.na(as.numeric(removed_sup)), as.numeric(removed_sup), numeric),
         data_type = if_else(!is.na(as.numeric(removed_sup)), 'numeric', data_type)) %>% # Remove the superscripts on the data.
  ungroup %>%
  filter(!grepl('Contents|Pcc', sheet)) %>% # Remove sheet with table of contents, and the "total" ones
  group_by(sheet) %>% # Do all ops on each sheet separately
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
  dat %>% 
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

freshfruit_tidy <- freshfruit_raw %>% 
  group_map(~ munge_one_tab(.x))

# Debug version
munged <- list()
sheetnames <- unique(freshfruit_raw$sheet)
for (i in 1:length(sheetnames)) {
  dat <- freshfruit_raw %>% ungroup %>% filter(sheet == sheetnames[i])
  munged[[i]] <- try(munge_one_tab(dat), TRUE)
}
map_chr(munged,class)

# test code below here ----------------------------------------------------

freshfruit_formats <- xlsx_formats(files[8])
freshfruit_raw %>% filter(sheet == 'Mangoes', address == 'A31') %>% pull(character_formatted)

zoooo <- freshfruit_raw %>%
  filter(!grepl('Contents|Pcc', sheet)) %>% # Remove sheet with table of contents, and the "total" ones
  group_by(sheet) %>% # Do all ops on each sheet separately
  mutate(is_data_row = row %in% (row[col == 1 & !is.na(numeric)])) %>% # Which rows have data in them
  filter(row <= max(row[is_data_row])) %>% # Get rid of rows below the data
  filter(col <= max(col[!is.na(numeric)])) %>% # Get rid of columns that go past the data
  select(sheet, row, col, data_type, numeric, character, is_data_row)

zeeee <- freshfruit_raw %>%
  rowwise %>%
  mutate(removed_sup = remove_superscripts(character_formatted),
         numeric = if_else(!is.na(as.numeric(removed_sup)), as.numeric(removed_sup), numeric),
         data_type = if_else(!is.na(as.numeric(removed_sup)), 'numeric', data_type)) %>% # Remove the superscripts on the data.
  ungroup %>%
  filter(!grepl('Contents|Pcc', sheet)) %>% # Remove sheet with table of contents, and the "total" ones
  group_by(sheet) %>% # Do all ops on each sheet separately
  mutate(is_data_row = row %in% (row[col == 1 & !is.na(numeric)])) %>% # Which rows have data in them
  filter(row <= max(row[is_data_row])) %>% # Get rid of rows below the data
  filter(col <= max(col[!is.na(numeric)])) %>% # Get rid of columns that go past the data
  select(sheet, row, col, data_type, numeric, character, is_data_row)

freshfruit_raw %>% filter(sheet == 'Mangoes', row == 31, col == 1)
zeeee %>% filter(sheet == 'Mangoes', row == 31, col == 1)
zoooo %>% filter(sheet == 'Mangoes', row == 31, col == 1)

foo <- zeeee %>% 
  group_map(~ munge_one_tab(.x))

munged <- list()
sheetnames <- unique(zeeee$sheet)
for (i in 1:length(sheetnames)) {
  dat <- zeeee %>% ungroup %>% filter(sheet == sheetnames[i])
  munged[[i]] <- try(munge_one_tab(dat), TRUE)
}
map_chr(munged,class)