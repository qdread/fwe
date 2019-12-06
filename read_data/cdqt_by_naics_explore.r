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

cdqt_naics_employees %>% filter(is.na(value) | D) %>% print(n = nrow(.)) # 145 of the 2754 rows have NA values.

# Trim down the NAICS names and worker type names to a shorter string

cdqt_naics_employees <- cdqt_naics_employees %>%
  mutate(NAICS = gsub('NAICS CLASSIFICATION: ', '', NAICS),
         NAICS = gsub('\\(|\\)', '', NAICS),
         worker_type = gsub(' - NUMBER OF WORKERS', '', worker_type))

# Ignore variations in labor type, sum them up
naics_empl_sums <- cdqt_naics_employees %>%
  group_by(level, FIPS, state_abbrev, state_name, NAICS) %>%
  summarize(n_workers = sum(value, na.rm = TRUE))

# Widen this DF for viewing purposes

naics_empl_sums %>%
  mutate(NAICS = gsub(' & ', '_', NAICS)) %>%
  pivot_wider(names_from = NAICS, names_prefix = 'NAICS_', values_from = n_workers, values_fill = list(n_workers = 0))

# Compare the number of workers numbers in here to the ones in the SUSB for non-agricultural goods.


# Tabulate income by NAICS ------------------------------------------------

cdqt_naics_income <- cdqt %>%
  filter(grepl('^NAICS', X14)) %>%
  filter(!grepl('AND', X14)) %>%
  filter(grepl("INCOME", X6))

unique(cdqt_naics_income$X6) # 32 variables, need to select the one(s) that are relevant.

cdqt_receipts <- cdqt_naics_income %>%
  filter(grepl('RECEIPTS', X6))

unique(cdqt_receipts$X6)

# Check whether farm-related receipts are total of all other groups
cdqt_natl_receipts <- cdqt_receipts %>% 
  filter(X9 == '99') %>%
  select(X6, X7, X14, X15) %>%
  mutate(X15 = gsub('[[:punct:]]', '', X15) %>% as.numeric) %>%
  pivot_wider(names_from = X6, values_from = X15)

receipt_sums <- apply(cdqt_natl_receipts[,c(6, 8, 10, 12, 14, 16, 18, 20)], 1, sum)

(receipt_sums - cdqt_natl_receipts[,4]) / cdqt_natl_receipts[,4] # They are basically the same with some small discrepancies of well under .1%.

cdqt_naics_totalreceipts <- cdqt %>%
  filter(grepl('^NAICS', X14)) %>%
  filter(!grepl('AND', X14)) %>%
  filter(X6 %in% "INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $") %>%
  select(X8, X9, X10, X11, X14, X15) %>%
  setNames(c('level', 'FIPS', 'state_abbrev', 'state_name', 'NAICS', 'value')) %>%
  mutate(D = value == '(D)',
         value = as.numeric(gsub('[[:punct:]]', '', value))) # Must store as numeric or big int.

cdqt_naics_totalreceipts %>% filter(is.na(value) | D) %>% print(n = nrow(.)) # 36 of the 691 rows have NA values.

# Trim down the NAICS names and worker type names to a shorter string

cdqt_naics_totalreceipts <- cdqt_naics_totalreceipts %>%
  mutate(NAICS = gsub('NAICS CLASSIFICATION: ', '', NAICS),
         NAICS = gsub('\\(|\\)', '', NAICS))

# Widen for viewing purposes.
cdqt_naics_receipts_wide <- cdqt_naics_totalreceipts %>%
  mutate(NAICS = gsub(' & ', '_', NAICS)) %>%
  pivot_wider(-D, names_from = NAICS, names_prefix = 'NAICS_', values_from = value, values_fill = list(value = NA))


# Combine employees and receipts, and export ------------------------------

cdqt_naics_data <- cdqt_naics_totalreceipts %>%
  rename(receipts = value) %>%
  select(-D) %>%
  full_join(naics_empl_sums) %>%
  arrange(NAICS, FIPS)

write_csv(cdqt_naics_data, file.path('/nfs/qread-data/cfs_io_analysis/NASS2012_receipts_workers_NAICS.csv'))
