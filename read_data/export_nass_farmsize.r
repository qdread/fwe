# Farm size x NAICS code from CDQT

library(tidyverse)

# Read text file ---------------------------------------------------

cdqt_file <- '/nfs/qread-data/raw_data/USDA/2012_cdqt_data.txt'

cdqt <- read_delim(cdqt_file, delim = '\t', col_names = FALSE, col_types = strrep('c', 15))


# Filter relevant rows ----------------------------------------------------

naics_x_farmsize <- cdqt %>%
  filter(X9 %in% '99') %>% # only care about US total
  filter(grepl('NAICS CLASSIFICATION', X14), grepl('ECONOMIC CLASS', X14)) %>%
  select(X6, X14, X15) %>%
  setNames(c('variable', 'classification', 'value')) %>%
  mutate(value = gsub('[[:punct:]]', '', value) %>% as.numeric)


# Separate classification into 2 ------------------------------------------

# For some reason classification sometimes has NAICS first and sometimes NAICS second

naics_x_farmsize <- naics_x_farmsize %>%
  separate(classification, into = c('c1','c2'), sep = ' AND ') %>%
  mutate(economic_class = if_else(grepl('ECONOMIC', c1), c1, c2),
         NAICS = if_else(grepl('NAICS', c1), c1, c2)) %>%
  select(variable, economic_class, NAICS, value)

# Clean up the names

naics_x_farmsize <- naics_x_farmsize %>%
  mutate(NAICS = gsub('NAICS CLASSIFICATION: ', '', NAICS),
         NAICS = gsub('\\(|\\)', '', NAICS),
         economic_class = gsub('ECONOMIC CLASS: ', '', economic_class),
         economic_class = gsub('\\(|\\)', '', economic_class))


# Widen and export --------------------------------------------------------

naics_x_farmsize_wide <- naics_x_farmsize %>%
  filter(grepl('FARM OPERATIONS', variable)) %>%
  pivot_wider(names_from = economic_class)

# Sort them manually so they're in increasing order.
naics_x_farmsize_wide <- naics_x_farmsize_wide[, c(1,2,13:3)]

write_csv(naics_x_farmsize_wide, '/nfs/qread-data/csv_exports/NASS_2012_NAICS_x_farmsize.csv')
