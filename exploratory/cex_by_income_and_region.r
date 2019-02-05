# EDA and plots with the summary data tabulated from BLS consumer expenditure (CEX) data
# QDR / FWE / 04 Feb 2019

fp <- ifelse(dir.exists('Z:'), 'Z:', '/nfs/fwe-data')

cex_byincome <- read.csv(file.path(fp, 'CEX/CEX sample R output/Integrated_mean_se_2017_byincomeclass.csv'), stringsAsFactors = FALSE)
cex_byregion <- read.csv(file.path(fp, 'CEX/CEX sample R output/Integrated_mean_se_2017_byregion.csv'), stringsAsFactors = FALSE)

library(tidyverse)
library(data.table)

# Reshape the data so that mean and se have their own columns, and the groups have their own rows
# Make sure order is preserved so that it's easier to look at the data in the intended order

cex_byregion <- cex_byregion %>% mutate(id = paste(group, title))

cex_by_regiongrp <- cex_byregion %>%
  group_by(group, estimate) %>%
  filter(!duplicated(title)) %>%
  ungroup %>%
  gather(region, value, -id, -title, -group, -estimate) %>%
  group_by(id, title, group, region) %>%
  spread(estimate, value) 

cex_by_regiongrp <- cex_by_regiongrp[order(match(cex_by_regiongrp$id, cex_byregion$id)), ] %>%
  ungroup %>%
  select(-id)

cex_byincome <- cex_byincome %>% mutate(id = paste(group, title))

cex_by_incomegrp <- cex_byincome %>%
  group_by(group, estimate) %>%
  filter(!duplicated(title)) %>%
  ungroup %>%
  gather(income_class, value, -id, -title, -group, -estimate) %>%
  group_by(id, title, group, income_class) %>%
  spread(estimate, value) 

cex_by_incomegrp <- cex_by_incomegrp[order(match(cex_by_incomegrp$id, cex_byincome$id)), ] %>%
  ungroup %>%
  select(-id)

cex_all <- cex_by_regiongrp %>%
  filter(grepl('All', region)) %>%
  ungroup %>%
  select(-region)

exp_all <- cex_all %>% filter(group %in% 'EXPEND')