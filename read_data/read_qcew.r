# Read Bureau of Labor Statistics QCEW (Qtrly census of employment and wages) data
# Only need US-wide 2012 annual averages for establishments of all sizes

library(tidyverse)
library(data.table)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
qcew12 <- fread(file.path(fp, 'Census/QCEW/2012.annual.singlefile.csv'))

# Size code is not included
qcew_us <- qcew12 %>%
  filter(area_fips %in% 'US000') %>%
  select(own_code, industry_code, agglvl_code, annual_avg_estabs, annual_avg_emplvl)

qcew_us_ag <- qcew_us %>%
  filter(grepl('^11*', industry_code))

qcew_us_othercrop <- qcew_us %>%
  filter(grepl('^1119', industry_code))

# 9469 "other crop": considering privately owned farms only
# not food: 456 tobacco, 3246 cotton, 1562 hay
# food: 293 sugarcane, 306 sugarbeet, 174 peanut
# mixture: 3433 other or combination crops. This includes grass seed farms. Assuming they are all equal, we get 12/28
# 12/28 * 3433 = 1471

(293 + 306 + 174 + 1471) / 9469
# 23.7% of this is food crops.

qcew_us_greenhouse <- qcew_us %>%
  filter(grepl('^1114', industry_code))

# 11141 is food, 11142 is not food.
(682+200+481)/8440
# 16.1% of this is food crops.