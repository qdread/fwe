# Assign individual LAFA categories to the groups they belong to
# QDR / FWE / 10 Feb. 2020

library(tidyverse)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub', '~'))

source(file.path(fp_github, 'fwe/read_data/read_lafa.r'))

lafa <- list('dairy', 'fat', 'fruit', 'grain', 'meat', 'sugar', 'veg') %>%
  map_dfr(~ data.frame(Group = ., Food = unique(get(.)$Category)))

write_csv(lafa, file.path('/nfs/qread-data', 'crossreference_tables/lafa_category_structure.csv'))
