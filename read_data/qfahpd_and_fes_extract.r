# Extract data from food expenditure series and quarterly food at home price database

# Food expenditure series -------------------------------------------------

fp <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'ERS/foodexpenditure')
dir(fp)

library(tidyverse)
library(tidyxl)
library(unpivotr)

# Nominal expenditure
nominal_exp_data <- xlsx_cells(file.path(fp, 'nominal_expenditures.xlsx'), sheets = 1, include_blank_cells = FALSE)
# Convert to longform.
# Get grouping variables for each data cell.
nominal_exp <- nominal_exp_data %>%
  filter(between(row, 3, 26)) %>%
  select(row, col, data_type, character, numeric) %>%
  behead('N', header1) %>%
  behead('N', header2) %>%
  behead('N', header3) %>%
  behead('W', Year) %>%
  select(Year, header1, header2, header3, numeric)

# Nominal expenditure without tax and tips  
nominal_exp_notaxestips_data <- xlsx_cells(file.path(fp, 'nominal_expenditures_no_taxes_tips.xlsx'), sheets = 1, include_blank_cells = FALSE)
# Convert to longform.
# Get grouping variables for each data cell.
nominal_exp_notaxestips <- nominal_exp_notaxestips_data %>%
  filter(between(row, 3, 26)) %>%
  select(row, col, data_type, character, numeric) %>%
  behead('N', header1) %>%
  behead('N', header2) %>%
  behead('N', header3) %>%
  behead('W', Year) %>%
  select(Year, header1, header2, header3, numeric)

# Constant-dollar expenditure
constant_dollar_exp_data <- xlsx_cells(file.path(fp, 'constant_dollar_expenditures.xlsx'), sheets = 1, include_blank_cells = FALSE)
# Convert to longform.
# Get grouping variables for each data cell.
constant_dollar_exp <- constant_dollar_exp_data %>%
  filter(between(row, 3, 26)) %>%
  select(row, col, data_type, character, numeric) %>%
  behead('N', header1) %>%
  behead('N', header2) %>%
  behead('N', header3) %>%
  behead('W', Year) %>%
  select(Year, header1, header2, header3, numeric)

# Constant-dollar expenditure without tax and tips  
constant_dollar_exp_notaxestips_data <- xlsx_cells(file.path(fp, 'constant_dollar_expenditures_no_taxes_tips.xlsx'), sheets = 1, include_blank_cells = FALSE)
# Convert to longform.
# Get grouping variables for each data cell.
constant_dollar_exp_notaxestips <- constant_dollar_exp_notaxestips_data %>%
  filter(between(row, 3, 26)) %>%
  select(row, col, data_type, character, numeric) %>%
  behead('N', header1) %>%
  behead('N', header2) %>%
  behead('N', header3) %>%
  behead('W', Year) %>%
  select(Year, header1, header2, header3, numeric)

# Normalized expenditure (proportion of income spent on food)
normalized_exp_data <- xlsx_cells(file.path(fp, 'normalized_food_expenditures.xlsx'), sheets = 1, include_blank_cells = FALSE)
# Convert to longform.
# Get grouping variables for each data cell.
normalized_exp <- normalized_exp_data %>%
  filter(between(row, 3, 27)) %>%
  select(row, col, data_type, character, numeric) %>%
  behead('N', header1) %>%
  behead('N', header2) %>%
  behead('N', header3) %>%
  behead('N', header4) %>%
  behead('W', Year) %>%
  select(Year, header1, header2, header3, numeric)  

# Food expenditure by final purchaser
exp_by_purchaser_data <- xlsx_cells(file.path(fp, 'food_expenditures_source_funds.xlsx'), sheets = 1, include_blank_cells = FALSE)
# Convert to longform.
# Get grouping variables for each data cell.
exp_by_purchaser <- exp_by_purchaser_data %>%
  filter(between(row, 3, 26)) %>%
  select(row, col, data_type, character, numeric) %>%
  behead('N', header1) %>%
  behead('N', header2) %>%
  behead('N', header3) %>%
  behead('N', header4) %>%
  behead('W', Year) %>%
  select(Year, header1, header2, header3, numeric)  

###
# Write all data produced above as CSVs (clean)

objs <- c('nominal_exp', 'nominal_exp_notaxestips', 'constant_dollar_exp', 'constant_dollar_exp_notaxestips', 'normalized_exp', 'exp_by_purchaser')
for (obj in objs) {
  write.csv(get(obj), file = file.path(fp, 'tidy_data', paste0(obj, '.csv')), row.names = FALSE)
}


# QFAHPD ------------------------------------------------------------------

# Data are a little bit "tidier" for this one so does not require as much cleaning.

# Version 1 includes upc codes and random-weight purchases, 1999-2006
# Version 2 is only upc coded purchases but has a couple extra groups (diet soda and coffee/tea), and covers 2004-2010.

fp <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'ERS/QFAHPD')
dir(fp)

library(tidyverse)
library(XLConnect)
library(zoo)

# Version 1

wb_fat1 <- loadWorkbook(file.path(fp, 'fatsandpreparedfoods_q1.xls'))
codes_fat1 <- readWorksheet(wb_fat1, 'codebook') %>%
  select(1:3) %>%
  setNames(c('group', 'code', 'desc')) %>%
  mutate(group = na.locf(group)) %>%
  filter(!is.na(desc))

data_fat1 <- map(getSheets(wb_fat1)[-(1:2)], ~ readWorksheet(wb_fat1, sheet = .x))
foodgroups_fat1 <- codes_fat1$desc[codes_fat1$group == 'Food Group'][match(as.numeric(getSheets(wb_fat1)[-(1:2)]), codes_fat1$code[codes_fat1$group == 'Food Group'])]
df_fat1 <- map2_dfr(data_fat1, foodgroups_fat1, ~ data.frame(foodgroup = .y, .x))

wb_fruitveg1 <- loadWorkbook(file.path(fp, 'fruitsandvegetables_q1.xls'))
data_fruitveg1 <- map(getSheets(wb_fruitveg1)[-(1:2)], ~ readWorksheet(wb_fruitveg1, sheet = .x))
foodgroups_fruitveg1 <- codes_fat1$desc[codes_fat1$group == 'Food Group'][match(as.numeric(getSheets(wb_fruitveg1)[-(1:2)]), codes_fat1$code[codes_fat1$group == 'Food Group'])]
df_fruitveg1 <- map2_dfr(data_fruitveg1, foodgroups_fruitveg1, ~ data.frame(foodgroup = .y, .x))

wb_graindairy1 <- loadWorkbook(file.path(fp, 'grainsanddairy_q1.xls'))
data_graindairy1 <- map(getSheets(wb_graindairy1)[-(1:2)], ~ readWorksheet(wb_graindairy1, sheet = .x))
foodgroups_graindairy1 <- codes_fat1$desc[codes_fat1$group == 'Food Group'][match(as.numeric(getSheets(wb_graindairy1)[-(1:2)]), codes_fat1$code[codes_fat1$group == 'Food Group'])]
df_graindairy1 <- map2_dfr(data_graindairy1, foodgroups_graindairy1, ~ data.frame(foodgroup = .y, .x))

wb_meategg1 <- loadWorkbook(file.path(fp, 'meatsandeggs_q1.xls'))
data_meategg1 <- map(getSheets(wb_meategg1)[-(1:2)], ~ readWorksheet(wb_meategg1, sheet = .x))
foodgroups_meategg1 <- codes_fat1$desc[codes_fat1$group == 'Food Group'][match(as.numeric(getSheets(wb_meategg1)[-(1:2)]), codes_fat1$code[codes_fat1$group == 'Food Group'])]
df_meategg1 <- map2_dfr(data_meategg1, foodgroups_meategg1, ~ data.frame(foodgroup = .y, .x))

# Version 2 (these are basically in the same format as Version 1)

wb_fat2 <- loadWorkbook(file.path(fp, 'qfahpd2fatsandpreparedfoods.xls'))
# Q2 codes are the same but with just the 2 other food groups added.
codes_q2 <- readWorksheet(wb_fat2, 'codebook') %>%
  select(1:3) %>%
  setNames(c('group', 'code', 'desc')) %>%
  mutate(group = na.locf(group)) %>%
  filter(!is.na(desc))

data_fat2 <- map(getSheets(wb_fat2)[-(1:2)], ~ readWorksheet(wb_fat2, sheet = .x))
foodgroups_fat2 <- codes_q2$desc[codes_q2$group == 'Food Group'][match(as.numeric(getSheets(wb_fat2)[-(1:2)]), codes_q2$code[codes_q2$group == 'Food Group'])]
df_fat2 <- map2_dfr(data_fat2, foodgroups_fat2, ~ data.frame(foodgroup = .y, .x))

wb_fruitveg2 <- loadWorkbook(file.path(fp, 'qfahpd2fruitsandvegetables.xls'))
data_fruitveg2 <- map(getSheets(wb_fruitveg2)[-(1:2)], ~ readWorksheet(wb_fruitveg2, sheet = .x))
foodgroups_fruitveg2 <- codes_q2$desc[codes_q2$group == 'Food Group'][match(as.numeric(getSheets(wb_fruitveg2)[-(1:2)]), codes_q2$code[codes_q2$group == 'Food Group'])]
df_fruitveg2 <- map2_dfr(data_fruitveg2, foodgroups_fruitveg2, ~ data.frame(foodgroup = .y, .x))

wb_graindairy2 <- loadWorkbook(file.path(fp, 'qfahpd2grainsanddairy.xls'))
data_graindairy2 <- map(getSheets(wb_graindairy2)[-(1:2)], ~ readWorksheet(wb_graindairy2, sheet = .x))
foodgroups_graindairy2 <- codes_q2$desc[codes_q2$group == 'Food Group'][match(as.numeric(getSheets(wb_graindairy2)[-(1:2)]), codes_q2$code[codes_q2$group == 'Food Group'])]
df_graindairy2 <- map2_dfr(data_graindairy2, foodgroups_graindairy2, ~ data.frame(foodgroup = .y, .x))

wb_meategg2 <- loadWorkbook(file.path(fp, 'qfahpd2meatsandeggs.xls'))
data_meategg2 <- map(getSheets(wb_meategg2)[-(1:2)], ~ readWorksheet(wb_meategg2, sheet = .x))
foodgroups_meategg2 <- codes_q2$desc[codes_q2$group == 'Food Group'][match(as.numeric(getSheets(wb_meategg2)[-(1:2)]), codes_q2$code[codes_q2$group == 'Food Group'])]
df_meategg2 <- map2_dfr(data_meategg2, foodgroups_meategg2, ~ data.frame(foodgroup = .y, .x))

###
# Combine all df's produced above, replace codes with text, and write to tidy CSVs

qfahpd1 <- rbind(df_fat1, df_fruitveg1, df_graindairy1, df_meategg1)
qfahpd2 <- rbind(df_fat2, df_fruitveg2, df_graindairy2, df_meategg2)

mktgps <- codes_fat1 %>% filter(group == 'Marketgroup ') %>% mutate(desc = trimws(desc))
divs <- codes_fat1 %>% filter(group == 'Division') %>% mutate(desc = trimws(desc))
regions <- codes_fat1 %>% filter(group == 'Region') %>% mutate(desc = trimws(desc))

qfahpd1 <- qfahpd1 %>%
  mutate(marketgroup = mktgps$desc[match(marketgroup, mktgps$code)],
         division = divs$desc[match(division, divs$code)],
         region = regions$desc[match(region, regions$code)])

qfahpd2 <- qfahpd2 %>%
  mutate(marketgroup = mktgps$desc[match(marketgroup, mktgps$code)],
         division = divs$desc[match(division, divs$code)],
         region = regions$desc[match(region, regions$code)])

write.csv(qfahpd1, file.path(fp, 'tidy_data/qfahpd1.csv'), row.names = FALSE)
write.csv(qfahpd2, file.path(fp, 'tidy_data/qfahpd2.csv'), row.names = FALSE)
