# Script to read in all data from supplemental information in Chaudhary et al 2015 (global land use characterization factors for species loss)

library(tidyxl)
library(tidyverse)
library(zoo)
library(unpivotr)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')

sheet_data <- xlsx_cells(file.path(fp, 'biodiversity/chaudhary2015SI/es5b02507_si_002.xlsx'))

# Which sheets to use, also all rows 1-3 are not used
sheet_data <- sheet_data %>%
  filter(grepl('regional|global', sheet), row > 3)
header_rows <- 4:6

# Fill merged cells in the header row group with cells either ahead or behind them
sheet_data <- sheet_data %>%
  group_by(sheet, row) %>%
  mutate(character = if_else(row %in% header_rows, na.locf(character, na.rm = FALSE), character)) %>%
  ungroup


### Do with unpivotr
sheet_data_behead <- sheet_data %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'W', name = ecoregion) %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'N', name = taxon) %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'N', name = landuse) %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'N', name = statistic) %>%
  select(sheet, ecoregion, taxon, landuse, statistic, numeric)

sheet_data_behead <- sheet_data_behead %>%
  rename(CF = sheet) %>%
  filter(!is.na(ecoregion)) %>%
  mutate_if(is.character, na.locf)

data_final <- sheet_data_behead %>%
  mutate(statistic = factor(statistic, labels = c('lower95ci', 'median', 'upper95ci'))) %>%
  spread(statistic, numeric)

write.csv(data_final, file.path(fp, 'biodiversity/chaudhary2015SI/chaud2015SI2.csv'), row.names = FALSE)
