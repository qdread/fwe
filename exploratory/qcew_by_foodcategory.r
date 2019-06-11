# Get proportions of food category for some of the BEA sectors from the QCEW data
# QDR / FWE / 11 June 2019

library(tidyverse)
library(data.table)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
qcew12 <- fread(file.path(fp, 'Census/QCEW/2012.annual.singlefile.csv'))

# Size code is not included
qcew_us <- qcew12 %>%
  filter(area_fips %in% 'US000') %>%
  select(own_code, industry_code, agglvl_code, annual_avg_estabs, annual_avg_emplvl)

qcew_us_ag <- qcew_us %>%
  filter(grepl('^(11)', industry_code))

# Fresh veg.
qcew_us_ag %>%
  filter(grepl('^(1112)', industry_code), agglvl_code == 18)

# Greenhouse.
qcew_us_ag %>%
  filter(grepl('^(1114)', industry_code), agglvl_code == 18)
qcew_us_ag %>%
  filter(grepl('^(1114)', industry_code), agglvl_code == 17)

# Tobacco, cotton, peanuts, and other crops.
qcew_us_ag %>%
  filter(grepl('^(1119)', industry_code), agglvl_code == 18) %>% group_by(industry_code) %>% summarize(emp = sum(annual_avg_emplvl))

# Poultry
qcew_us_ag %>%
  filter(grepl('^(1123)', industry_code), agglvl_code == 18) 

# Animal farms and aquaculture
qcew_us_ag %>%
  filter(grepl('^(112)', industry_code), agglvl_code == 18) %>% group_by(industry_code) %>% summarize(emp = sum(annual_avg_emplvl))

# Fish and game
qcew_us_ag %>%
  filter(grepl('^(114)', industry_code), agglvl_code == 18) %>% group_by(industry_code) %>% summarize(emp = sum(annual_avg_emplvl))