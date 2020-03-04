# All-intervention analysis (preliminary "messy" analysis before putting into clean Rmd document)
# QDR / FWE / 24 Feb 2020


# Standardized date labeling ----------------------------------------------

# One-time costs for two scenarios: no coordination and coordination
# Get them from the document

library(tidyverse)
library(readxl)
library(zoo)

is_local <- dir.exists('Q:/')
fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_github <- file.path(ifelse(is_local, '~/Documents/GitHub', '~'))

datelabel_costs <- read_xlsx(file.path(fp, 'scenario_inputdata/date_labeling/costs for date labeling change.xlsx'), skip = 9)

datelabel_costs_nocoord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (w/o coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))
datelabel_costs_coord <- datelabel_costs[which(datelabel_costs[,1] == "Total costs for date labeling (with coordination)"), 2:4] %>% t %>% c %>% setNames(c('lower','mean','upper'))

# We have one-time costs but need to annualize using 5 years at 7% interest as was done for waste tracking.

# Annuity function as implemented in excel, f and t are zero.
pmt <- function(p, r, n, f, t) (p * r * (1+r)^n  - f) / (((1+r)^n - 1) * (1 + r * t))

(datelabel_costs_nocoord_annual <- pmt(datelabel_costs_nocoord, r = 0.07, n = 5, f = 0, t = 0)) # 350m to 1.4b
(datelabel_costs_coord_annual <- pmt(datelabel_costs_coord, r = 0.07, n = 5, f = 0, t = 0)) # 33m to 286m

# Go back into the reformulation model output to see what portion of the total cost of the intervention is equipment, so we can get offsetting impact.
# For each model run, this will be in the tab "Detailed Cost" and each product category has a materials cost per UPC and total materials cost.
# The costs are split into labor and materials so we need to pull out the materials rows only.

### Do this with readxl

### 0% coordination
modelrun0 <- read_xlsx(file.path(fp, 'scenario_inputdata/date_labeling/date labeling model run-0% coordination.xlsx'), sheet = 'Detailed Costs', col_names = FALSE)

# carry over the first row to fill in the empty cells, then paste together rows 1 and 2 names to create the true header.
header_names <- gsub('NA_', '', paste(na.locf(unlist(modelrun0[1,]), na.rm = FALSE), unlist(modelrun0[2,]), sep = '_') )

modelrun0 <- modelrun0 %>%
  slice(-(1:2)) %>%
  setNames(header_names) %>%
  mutate_at(1:3, na.locf)

modelrun0_materials <- modelrun0 %>%
  filter(Cost_Type %in% 'Materials') %>%
  select(`Product Category`:`Brand Type`, Total_5th:Total_95th) %>%
  mutate_at(vars(Total_5th:Total_95th), as.numeric)

# Totals across all the food types
modelrun0_materials %>% select(Total_5th:Total_95th) %>% colSums # 330M to 789M

### 100% coordination
modelrun100 <- read_xlsx(file.path(fp, 'scenario_inputdata/date_labeling/date labeling model run-100% coordination.xlsx'), sheet = 'Detailed Costs', col_names = FALSE)

# carry over the first row to fill in the empty cells, then paste together rows 1 and 2 names to create the true header.
header_names <- gsub('NA_', '', paste(na.locf(unlist(modelrun100[1,]), na.rm = FALSE), unlist(modelrun100[2,]), sep = '_') )

modelrun100 <- modelrun100 %>%
  slice(-(1:2)) %>%
  setNames(header_names) %>%
  mutate_at(1:3, na.locf)

modelrun100_materials <- modelrun100 %>%
  filter(Cost_Type %in% 'Materials') %>%
  select(`Product Category`:`Brand Type`, Total_5th:Total_95th) %>%
  mutate_at(vars(Total_5th:Total_95th), as.numeric)

# Totals across all the food types
modelrun100_materials %>% select(Total_5th:Total_95th) %>% colSums # 0, because all of it is labor costs in this model.
