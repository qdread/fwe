# Process US MSA data
# QDR / FWE / 20 Feb 2020

library(tidyverse)
library(readxl)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')

msas <- read_xls(file.path(fp, 'scenario_inputdata/metroareas_sep2018.xls'), skip = 2) %>%
  filter(!is.na(`FIPS State Code`))

table(msas$`Metropolitan/Micropolitan Statistical Area`)

msas %>% 
  filter(`Metropolitan/Micropolitan Statistical Area` == 'Metropolitan Statistical Area') %>%
  pull(`CBSA Title`) %>%
  unique

# There are 392 metropolitan statistical areas in the US, across 1251 counties.

# look at Seattle

msas %>% filter(grepl('Seattle', `CSA Title`))
