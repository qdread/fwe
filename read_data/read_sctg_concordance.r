# Read the xlsx crosswalk for NAICS/BEA/SCTG into R

fp_crosswalk <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')

library(tidyverse)
library(XLConnect)

crosswalk_wb <- loadWorkbook(file.path(fp_crosswalk, 'NAICS_BEA_SCTG Crosswalk.xlsx'))
concordance <- readWorksheet(crosswalk_wb, sheet = 'NAICS-IO-SCTG Concordance', startRow = 3)

# Cut off the extra rows
lastrow <- which(is.na(concordance$BEA.Code.and.Title))[1] - 1
concordance <- concordance[1:lastrow,]


# Get proportions for the 3 with multiple SCTG ----------------------------

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
cs <- c('factor','numeric','character')
susb12 <- read.csv(file.path(fp, 'Census/SUSB/us_state_6digitnaics_2012.txt'), colClasses = cs[c(1,1,1,2,2,2,1,1,2,1,2,1,3,3,3)]) 
susb12US <- susb12 %>% filter(STATEDSCR == 'United States', ENTRSIZEDSCR == 'Total')

susb12US %>% filter(grepl('^2123', NAICS)) # mining industries.
susb12US %>% filter(grepl('^32411', NAICS)) # petroleum refineries -- cannot be disaggregated.

# at this point probably OK to use the one provided in the concordance.


# Tidy up concordance -----------------------------------------------------

concordance <- concordance %>%
  as_tibble %>%
  select(-Misc.Notes) %>%
  rename(BEA_def = Col4) %>%
  mutate(related_2007_NAICS_codes = pmap(concordance %>% select(Related.2007.NAICS.Codes:Col13), c) %>% map(~ .x[!is.na(.x)])) %>%
  select(-(Related.2007.NAICS.Codes:Col13))

# Write to a .RData file so the tibble can be loaded as is
save(concordance, file = file.path(fp_crosswalk, 'NAICS_BEA_SCTG_crosswalk.RData'))
