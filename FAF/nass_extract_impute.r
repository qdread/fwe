# NASS census of ag CDQT Data extraction by state and NAICS
# Get as many variables as possible, using imputation to fill in the suppressed records.
# QDR / FWE / 09 Dec 2019

library(tidyverse)

# Read text file ---------------------------------------------------

cdqt_file <- '/nfs/qread-data/raw_data/USDA/2012_cdqt_data.txt'

# Columns are delimited by tabs? Probably
# Read all as characters to begin with.
cdqt <- read_delim(cdqt_file, delim = '\t', col_names = FALSE, col_types = strrep('c', 15))


# Get all labor and income variables by NAICS -----------------------------

# All variables that are classified by state x NAICS code under operations, labor, and income
# Get rid of duplicates, set row names, convert NAICS into a short character
# Convert value column to numeric, setting suppressed rows to NA.
cdqt_naics <- cdqt %>%
  filter(grepl('^NAICS', X14)) %>%
  filter(!grepl('AND', X14)) %>%
  filter(grepl('FARM OPERATIONS|LABOR|INCOME', X6)) %>%
  filter(!grepl('FARM OPERATIONS, ORGANIZATION', X6, fixed = TRUE)) %>% # this seems fairly unnecessary.
  filter(!grepl('INCOME, NET CASH FARM', X6, fixed = TRUE)) %>% # also not necessary I think.
  filter(X8 %in% c('STATE', 'NATIONAL')) %>%
  select(X6, X7, X8, X9, X10, X11, X14, X15) %>%
  setNames(c('variable','category','level', 'state_fips', 'state_abbrev', 'state_name', 'NAICS', 'value')) %>%
  filter(!duplicated(.)) %>%
  mutate(NAICS = gsub('NAICS CLASSIFICATION: ', '', NAICS),
         NAICS = gsub('\\(|\\)', '', NAICS))

tail(sort(unique(cdqt_naics$value))) # Both D and Z codes are included here. 
sum(cdqt_naics$value == '(D)') # about 1600 values. Not disclosed for confidentiality. Should be imputed.
sum(cdqt_naics$value == '(Z)') # 35 values. These are effectively zero values. Can set them to 0.

cdqt_naics <- cdqt_naics %>%
  mutate(
    suppressed = value == '(D)',
    value = case_when(
      value == '(D)' ~ as.numeric(NA),
      value == '(Z)' ~ 0,
      TRUE ~ as.numeric(gsub(',', '', value))
    ))

unique(cdqt_naics$variable) # 34 variables. Some are sums of other variables.
unique(cdqt_naics$NAICS) # this has all the ag naics codes.

# Reduce the number of variables to a final list of a few important ones.
vars_to_use <- c('FARM OPERATIONS - NUMBER OF OPERATIONS', 
                 'LABOR, HIRED - EXPENSE, MEASURED IN $',
                 'LABOR, CONTRACT - EXPENSE, MEASURED IN $',
                 'INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $',
                 'LABOR, HIRED - NUMBER OF WORKERS')
vars_shortnames <- c('n_operations', 'labor_hired_expense', 'labor_contract_expense', 'receipts', 'n_workers')


cdqt_naics <- cdqt_naics %>%
  filter(variable %in% vars_to_use) %>%
  mutate(variable = vars_shortnames[match(variable, vars_to_use)])

# Widen this data frame by state so that each variable has a column within state and NAICS

cdqt_naics_wide <- cdqt_naics %>%
  select(-category, -suppressed) %>%
  pivot_wider(names_from = variable, values_from = value, values_fill = list(value = 0))

# Get rid of the NAICS codes entirely that have no data other than number of operations.
naicstoremove <- cdqt_naics_wide %>% group_by(NAICS) %>%
  summarize(no_data = all(labor_hired_expense == 0 & labor_contract_expense == 0 & receipts == 0 & n_workers == 0))

cdqt_naics_wide <- cdqt_naics_wide %>%
  filter(!NAICS %in% naicstoremove$NAICS[naicstoremove$no_data]) # There are 184 missing values out of >3000 so can be imputed.

# Repivot to long form.
cdqt_naics <- cdqt_naics_wide %>%
  pivot_longer(n_operations:n_workers, names_to = 'variable')

# We cannot impute when done by state row because there are too many variables.

# Use MICE to create some imputations of this dataset ---------------------

library(mice)
cdqt_to_impute <- cdqt_naics_wide %>%
  filter(!level %in% 'NATIONAL') %>%
  select(state_abbrev, NAICS, labor_hired_expense:n_workers) %>%
  mutate(state_abbrev = factor(state_abbrev), NAICS = factor(NAICS)) %>%
  mutate_at(vars(labor_hired_expense:n_workers), ~ log(. + 1))

cdqt_imputed <- mice(data = as.data.frame(cdqt_to_impute), m = 10, seed = 222, method = 'rf')  # Random forest imputation.

cdqt_imputed_complete <- complete(cdqt_imputed) %>%
  mutate_at(vars(labor_hired_expense:n_workers), ~ round(exp(.) - 1)) %>%
  rename_at(vars(labor_hired_expense:n_workers), ~ paste(., 'imputed', sep = '_'))


# Join imputed with real data and replace ---------------------------------

# Only replace the ones that are needed to replace

cdqt_naics_wide_withimp <- cdqt_naics_wide %>%
  left_join(cdqt_imputed_complete)

# Manipulate this wide DF to long form to put the imputed column side by side with the non imputed column.
cdqt_naics_withimp <- cdqt_naics_wide_withimp %>%
  pivot_longer(n_operations:n_workers_imputed) %>%
  mutate(imputed = if_else(grepl('imputed', name), 'imputed', 'original'), name = gsub('_imputed', '', name)) %>%
  pivot_wider(names_from = imputed)

# If there is no original value, replace it with the imputed value
cdqt_naics_withimp <- cdqt_naics_withimp %>%
  mutate(value = if_else(is.na(original), imputed, original)) %>%
  select(-original, -imputed) %>%
  pivot_wider()


# Export imputed dataframe ------------------------------------------------

cdqt_naics_withimp %>%
  arrange(NAICS, state_fips) %>%
  write_csv('/nfs/qread-data/cfs_io_analysis/NASS2012_receipts_workers_NAICS_imputed.csv')

cdqt_naics_data <- cdqt_naics_totalreceipts %>%
  rename(receipts = value) %>%
  select(-D) %>%
  full_join(naics_empl_sums) %>%
  arrange(NAICS, FIPS)
