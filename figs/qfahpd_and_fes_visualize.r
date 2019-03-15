# Visualize patterns in the FES data

library(tidyverse)

fp <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'ERS/foodexpenditure/tidy_data')

walk(dir(fp, full.names = TRUE), ~ assign(tools::file_path_sans_ext(basename(.x)), read.csv(.x, stringsAsFactors = FALSE)))

# Comparison with eeio sectors
foodstores <- 1.907e11
fullservicerestos <- 2.184e11
limitedservicerestos <- 2.778e11
allotherfoodestablishments <- 0.867e11

# Nominal expenditures on food

# Food at home
nominal_exp %>% filter(Year == 2012, header1 == 'Food at home (FAH)')
nominal_exp %>% filter(Year == 2012, header1 == 'Total FAH')
nominal_exp %>% filter(Year == 2012, header1 == 'Food away from home (FAFH)')
nominal_exp %>% filter(Year == 2012, header1 == 'Total FAFH')

# QFAHPD data
# -----------

fp <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'ERS/QFAHPD/tidy_data')
qfahpd1 <- read.csv(file.path(fp, 'qfahpd1.csv'), stringsAsFactors = FALSE)
qfahpd2 <- read.csv(file.path(fp, 'qfahpd2.csv'), stringsAsFactors = FALSE)

# Prices per 100 grams of different foods -- aggregate by region
qfahpd2 %>% 
  filter(year == 2010, quarter == 4) %>%
  group_by(foodgroup, region) %>%
  summarize(price = weighted.mean(price, aggweight, na.rm = TRUE))
