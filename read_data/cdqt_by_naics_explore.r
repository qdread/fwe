# USDA Census of agriculture - see if we can tabulate the data by NAICS code by state or county

library(tidyverse)


# Read text file ---------------------------------------------------


cdqt_file <- '/nfs/fwe-data/USDA/2012_cdqt_data.txt'

# Columns are delimited by tabs? Probably
# Read all as characters to begin with.
cdqt <- read_delim(cdqt_file, delim = '\t', col_names = FALSE, col_types = strrep('c', 15))


# Explore categorical columns ---------------------------------------------

x5u <- unique(cdqt$X5)
x6u <- unique(cdqt$X6)
x7u <- unique(cdqt$X7)
x8u <- unique(cdqt$X8) # national, state, and county.

x12u <- unique(cdqt$X12)
x13u <- unique(cdqt$X13)

# X6 seems to be the most detailed thing

cdqt_natl_econ <- cdqt %>% filter(X8 %in% 'NATIONAL', X5 %in% 'ECONOMICS')

unique(cdqt_natl_econ$X6)
unique(cdqt_natl_econ$X14)

cdqt_naics <- cdqt %>% filter(grepl('NAICS', X14))

classifs <- unique(cdqt_naics$X14)

# Find anything that is used to tabulate within NAICS codes
# Get rid of anything within parens
classifs_short <- gsub('\\([^)]*\\)', '', classifs)
unique(classifs_short)

grep('LABOR', classifs, value = TRUE)
grep('OPERATORS', classifs, value = TRUE)
grep('ECONOMIC CLASS', classifs, value = TRUE)

cdqt_justnaics <- cdqt %>% filter(grepl('^NAICS', X14), !grepl('AND', X14))
unique(cdqt_justnaics$X14)
unique(cdqt_justnaics$X6)


# Tabulate number of workers by NAICS -------------------------------------

cdqt_naics_employees <- cdqt %>%
  filter(grepl('^NAICS', X14)) %>%
  filter(!grepl('AND', X14)) %>%
  filter(grepl("NUMBER OF WORKERS", X6))

table(cdqt_naics_employees$X6)
table(cdqt_naics_employees$X12)
table(cdqt_naics_employees$X13)
table(cdqt_naics_employees$X7)

# We now have 4 classifications of workers for each state and for each NAICS code!

cdqt_naics_employees <- cdqt_naics_employees %>%
  select(X6, X8, X9, X10, X11, X14, X15) %>%
  setNames(c('worker_type', 'level', 'FIPS', 'state_abbrev', 'state_name', 'NAICS', 'value'))

# Convert NAICS code column to just codes, and value column to numeric

sort(unique(cdqt_naics_employees$value)) %>% tail # It contains a D or an integer.

cdqt_naics_employees <- cdqt_naics_employees %>%
  mutate(D = value == '(D)',
         value = as.integer(gsub('[[:punct:]]', '', value)))

cdqt_naics_employees %>% filter(is.na(value) | D) %>% print(n = nrow(.))
