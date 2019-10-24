# Process raw 2012 QCEW first quarter data, separated by business size class
# QDR / FWE / 24 Oct 2019

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')

# See whether CBP has ag codes
cbp12co <- read_csv(file.path(fp, 'Census/CBP/cbp12co.txt'))
grep('^11', unique(cbp12co$naics), value=T) # No it doesn't.

qcew12size <- read_csv(file.path(fp, 'Census/QCEW/2012.q1.by_size.csv'))

qcew12 <- read_csv(file.path(fp, 'Census/QCEW/2012.annual.singlefile.csv'))

# See https://data.bls.gov/cew/doc/titles/agglevel/agglevel_titles.htm for details on how this is split up
# National level, by size class, only private ownership, and different levels of NAICS aggregation
qcew12size <- qcew12size %>%
  filter(agglvl_code %in% 24:28)

# At least tabulate the number of establishments with QCEW
qcew12size_less <- qcew12size %>%
  mutate(emplvl = apply(cbind(month1_emplvl, month2_emplvl, month3_emplvl), 1, mean, na.rm = TRUE),
         size_title = gsub(' employees per establishment', '', size_title),
         size_title = factor(size_title, levels = size_title[1:9]),
         industry_code = paste0(industry_code, map_chr(6 - nchar(industry_code), ~ strrep('-', .)))) %>%
  select(industry_code, industry_title, agglvl_code, agglvl_title, size_code, size_title, emplvl, qtrly_estabs_count)

qcew12_n_estab <- qcew12size_less %>%
  select(-emplvl, -size_code, -agglvl_code) %>%
  spread(size_title, qtrly_estabs_count, fill = 0)
qcew12_n_emp <- qcew12size_less %>%
  select(-qtrly_estabs_count, -size_code, -agglvl_code) %>%
  spread(size_title, emplvl, fill = 0)

# We want number of firms and number of establishments 
# This can be done for everything except agriculture with SUSB

susb12 <- read_csv(file.path(fp, 'Census/SUSB/us_state_6digitnaics_2012.txt'))
susb12_us <- susb12 %>%
  filter(STATE %in% '00') %>%
  select(NAICS, NAICSDSCR, ENTRSIZE, ENTRSIZEDSCR, FIRM, ESTB, EMPL_N, PAYR_N, RCPT_N) %>%
  mutate(ENTRSIZEDSCR = gsub('-', ' to ', ENTRSIZEDSCR),
         ENTRSIZEDSCR = factor(ENTRSIZEDSCR, levels = ENTRSIZEDSCR[1:9])) # Ensure order is the same

wide_table <- function(dat, val_col) {
  val_col <- enquo(val_col)
  dat %>%
    select(NAICS, NAICSDSCR, ENTRSIZEDSCR, !!val_col) %>%
    spread(ENTRSIZEDSCR, !!val_col, fill = 0)
}

susb_n_firms <- susb12_us %>% wide_table(FIRM)
susb_n_estabs <- susb12_us %>% wide_table(ESTB)
susb_n_employees <- susb12_us %>% wide_table(EMPL_N)
susb_n_payroll <- susb12_us %>% wide_table(PAYR_N)
susb_n_rcpts <- susb12_us %>% wide_table(RCPT_N)

# Write both sets of data to CSV
fp_out <- '/nfs/qread-data/csv_exports'
write_csv(qcew12_n_estab, file.path(fp_out, 'QCEW_n_establishments_NAICS_by_size.csv'))
write_csv(qcew12_n_emp, file.path(fp_out, 'QCEW_n_employees_NAICS_by_size.csv'))
write_csv(susb_n_firms, file.path(fp_out, 'SUSB_n_firms_NAICS_by_size.csv'))
write_csv(susb_n_estabs, file.path(fp_out, 'SUSB_n_establishments_NAICS_by_size.csv'))
write_csv(susb_n_employees, file.path(fp_out, 'SUSB_n_employees_NAICS_by_size.csv'))
write_csv(susb_n_payroll, file.path(fp_out, 'SUSB_payroll_NAICS_by_size.csv'))
write_csv(susb_n_rcpts, file.path(fp_out, 'SUSB_receipts_NAICS_by_size.csv'))
