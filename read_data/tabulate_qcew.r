# Process raw 2012 QCEW first quarter data, separated by business size class
# QDR / FWE / 24 Oct 2019

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')
fp_crosswalks <- ifelse(dir.exists('Q:/'), 'Q:/crossreference_tables', '/nfs/qread-data/crossreference_tables')

# See whether CBP has ag codes
# cbp12co <- read_csv(file.path(fp, 'Census/CBP/cbp12co.txt'))
# grep('^11', unique(cbp12co$naics), value=T) # No it doesn't.


# QCEW --------------------------------------------------------------------

# Mapping of QCEW size codes to our "harmonized" four size classes
harmonized_size_classes <- c('fewer than 20', '20 to 99', '100 to 499', 'more than 500')

qcew12size <- read_csv(file.path(fp, 'Census/QCEW/2012.q1.by_size.csv'))

# See https://data.bls.gov/cew/doc/titles/agglevel/agglevel_titles.htm for details on how this is split up
# National level, by size class, only private ownership, and different levels of NAICS aggregation
qcew12size <- qcew12size %>%
  filter(agglvl_code %in% 24:28)

# Tabulate the number of establishments with QCEW
qcew12size_less <- qcew12size %>%
  mutate(size_class = factor(cut(size_code, breaks = c(1, 3, 5, 7, 9), include.lowest = TRUE), labels = harmonized_size_classes),
         emplvl = apply(cbind(month1_emplvl, month2_emplvl, month3_emplvl), 1, mean, na.rm = TRUE),
         industry_code = paste0(industry_code, map_chr(6 - nchar(industry_code), ~ strrep('-', .)))) %>%
  select(industry_code, industry_title, agglvl_code, agglvl_title, size_class, emplvl, qtrly_estabs_count) %>%
  group_by(industry_code, industry_title, agglvl_code, agglvl_title, size_class) %>% # Sum up the reduced-resolution size classes
  summarize_all(sum) %>%
  ungroup

qcew12_n_estab <- qcew12size_less %>%
  select(-emplvl, -agglvl_code) %>%
  pivot_wider(names_from = size_class, values_from = qtrly_estabs_count, values_fill = list(qtrly_estabs_count = 0)) %>%
  mutate(total = apply(.[,4:7], 1, sum))
qcew12_n_emp <- qcew12size_less %>%
  select(-qtrly_estabs_count, -agglvl_code) %>%
  pivot_wider(names_from = size_class, values_from = emplvl, values_fill = list(emplvl = 0)) %>%
  mutate(total = apply(.[,4:7], 1, sum))

# SUSB --------------------------------------------------------------------

# We want number of firms and number of establishments 
# This can be done for everything except agriculture with SUSB

# This already has the harmonized size class mapping

susb12 <- read_csv(file.path(fp, 'Census/SUSB/us_state_6digitnaics_2012.txt'))
susb12_us <- susb12 %>%
  filter(STATE %in% '00', ENTRSIZE %in% c('01','05','06','07','09')) %>%
  select(NAICS, NAICSDSCR, ENTRSIZE, FIRM, ESTB, EMPL_N, PAYR_N, RCPT_N) %>%
  mutate(ENTRSIZE = factor(ENTRSIZE, labels = c('total', harmonized_size_classes)))

wide_table <- function(dat, val_col) {
  val_col <- enquo(val_col)
  dat %>%
    select(NAICS, NAICSDSCR, ENTRSIZE, !!val_col) %>%
    spread(ENTRSIZE, !!val_col, fill = 0)
}

susb_n_firms <- susb12_us %>% wide_table(FIRM)
susb_n_estabs <- susb12_us %>% wide_table(ESTB)
susb_n_employees <- susb12_us %>% wide_table(EMPL_N)
susb_n_payroll <- susb12_us %>% wide_table(PAYR_N)
susb_n_rcpts <- susb12_us %>% wide_table(RCPT_N)


# Map QCEW to BEA and FAO groups -----------------------------------------------

# Read crosswalk that maps NAICS 07 and NAICS 12 to the BEA codes
bea_naics <- read_csv(file.path(fp_crosswalks, 'BEA_NAICS07_NAICS12_crosswalk.csv'))

# Unique codes in the QCEW dataset
qcew_naics <- unique(gsub('-','',qcew12size_less$industry_code))

# First get rid of any redundant ones in the QCEW that have a longer and more specific code.
qcew_redundant <- map_lgl(qcew_naics, ~ nchar(.) < max(nchar(grep(paste0('^', .), qcew_naics, value = TRUE))))
qcew_naics_notredundant <- qcew_naics[!qcew_redundant]

# Check which codes are in the crosswalk and which aren't
intersect(qcew_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(qcew_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(bea_naics$related_2012_NAICS_6digit, qcew_naics_notredundant)

# combine them so that any "more specific" one is matched to its less specific parent code.
qcew_naics_matchidx <- map_int(qcew_naics_notredundant, function(code) {
  subcodes <- map(2:nchar(code), ~ substr(code, 1, .)) # all possible subcodes
  match_idx <- map(subcodes, ~ grep(paste0('^', .), bea_naics$related_2012_NAICS_6digit))
  # Find the longest matching code
  longest_match <- max(which(map_int(match_idx, length) > 0))
  ifelse(longest_match > 0, match_idx[[longest_match]], NA)
})

# Get BEA codes corresponding to the matches
qcew_bea_lookup <- data.frame(industry_code = qcew_naics_notredundant, 
                              BEA_code = bea_naics$BEA_Code[qcew_naics_matchidx])

# Remove redundant rows from the QCEW dataset and add column for BEA code
qcew_bea <- qcew12size_less %>%
  filter(industry_code %in% qcew_naics_notredundant, agglvl_code == 28) %>%
  left_join(qcew_bea_lookup) %>%
  group_by(BEA_code, size_class) %>%
  summarize(emplvl = sum(emplvl), qtrly_estabs_count = sum(qtrly_estabs_count))

# Correct the 2007 NAICS codes in this data frame to 2012 codes.

# Next, get the FAO food type weights and get the proportion of each food type in each BEA code multiplied by the number of employees/establishments.
food_crosswalk <- read_csv(file.path(fp_crosswalks, 'naics_crosswalk_final.csv')) %>%
  select(BEA_389_code, BEA_389_def, proportion_food, cereals:beverages) %>%
  rename(BEA_code = BEA_389_code, BEA_title = BEA_389_def) %>% 
  filter(proportion_food > 0)

# Make a long version with a separate row for each food.
# Modified 30 Oct: don't multiply by proportion food but include it just in case.
qcew_food <- qcew_bea %>%
  inner_join(food_crosswalk) %>%
  pivot_longer(cereals:beverages, names_to = 'food_type', values_to = 'food_type_weight') %>%
  mutate(employment_weighted = emplvl * food_type_weight,
         establishments_weighted = qtrly_estabs_count * food_type_weight) %>%
  select(BEA_code, BEA_title, proportion_food, size_class, food_type, employment_weighted, establishments_weighted)

# Keep only food codes but maintain 4 rows for each food code, even if some of the rows have all zeroes.
qcew_allvars_byfood <- qcew_food %>%
  pivot_longer(employment_weighted:establishments_weighted, names_to = 'variable') %>%
  pivot_wider(names_from = size_class, values_fill = list(value = 0)) %>%
  pivot_longer(`fewer than 20`:`more than 500`, names_to = 'size_class') %>%
  pivot_wider(names_from = food_type) %>%
  arrange(variable, BEA_code)

# Map SUSB to BEA & FAO ---------------------------------------------------

# Unique codes in the SUSB dataset
susb_naics <- unique(gsub('-','', susb12_us$NAICS))

# First get rid of any redundant ones in the QCEW that have a longer and more specific code.
susb_redundant <- map_lgl(susb_naics, ~ nchar(.) < max(nchar(grep(paste0('^', .), susb_naics, value = TRUE))))
susb_naics_notredundant <- susb_naics[!susb_redundant] # Some of these are actually less than 6 characters.

# See if the <6 characters are in FSC.
shortcodes <- susb_naics_notredundant[nchar(susb_naics_notredundant) < 6]
susb12_us %>% filter(NAICS %in% c('44-45','48-49')) # We can ignore these.

susb_naics_notredundant <- susb_naics_notredundant[nchar(susb_naics_notredundant) == 6]


# Check which codes are in the crosswalk and which aren't
intersect(susb_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(susb_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(bea_naics$related_2012_NAICS_6digit, susb_naics_notredundant) # The primary ag codes unfortunately.
susb12_us %>% filter(NAICS %in% c('517911','517919')) # Not relevant so OK.

# combine them so that any "more specific" one is matched to its less specific parent code.
susb_naics_matchidx <- map_int(susb_naics_notredundant, function(code) {
  subcodes <- map(2:nchar(code), ~ substr(code, 1, .)) # all possible subcodes
  match_idx <- map(subcodes, ~ grep(paste0('^', .), bea_naics$related_2012_NAICS_6digit))
  # Find the longest matching code
  longest_match <- max(which(map_int(match_idx, length) > 0))
  ifelse(longest_match > 0, match_idx[[longest_match]], NA)
})

# Get BEA codes corresponding to the matches
susb_bea_lookup <- data.frame(NAICS = susb_naics_notredundant, 
                              BEA_code = bea_naics$BEA_Code[susb_naics_matchidx])

# Remove redundant rows from the SUSB dataset and add column for BEA code
susb_bea <- susb12_us %>%
  filter(NAICS %in% susb_naics_notredundant) %>%
  left_join(susb_bea_lookup) %>%
  select(-NAICS, -NAICSDSCR) %>%
  group_by(BEA_code, ENTRSIZE) %>%
  summarize_all(sum)

# Make a long version with a separate row for each food.
# Modified 30 Oct: don't multiply by proportion food but include it just in case.
susb_food <- susb_bea %>%
  inner_join(food_crosswalk) %>%
  pivot_longer(cereals:beverages, names_to = 'food_type', values_to = 'food_type_weight') %>%
  mutate_at(vars(FIRM:RCPT_N), ~ . * food_type_weight) %>%
  ungroup %>%
  select(BEA_code, BEA_title, proportion_food, ENTRSIZE, food_type, FIRM:RCPT_N) %>%
  setNames(c('BEA_code','BEA_title','proportion_food', 'size_class','food_type','firms_weighted','establishments_weighted','employment_weighted','payroll_weighted','receipts_weighted'))

# Keep only food codes but maintain 4 rows for each food code, even if some of the rows have all zeroes.
susb_allvars_byfood <- susb_food %>%
  pivot_longer(firms_weighted:receipts_weighted, names_to = 'variable') %>%
  pivot_wider(names_from = size_class, values_fill = list(value = 0)) %>%
  pivot_longer(`total`:`more than 500`, names_to = 'size_class') %>%
  pivot_wider(names_from = food_type) %>%
  arrange(variable, BEA_code)


# Export ------------------------------------------------------------------


# Write both sets of data to CSV
fp_out <- '/nfs/qread-data/csv_exports'
write_csv(qcew_allvars_byfood, file.path(fp_out, 'QCEW_by_food_type.csv'))
write_csv(susb_allvars_byfood, file.path(fp_out, 'SUSB_by_food_type.csv'))

# write_csv(qcew12_n_estab, file.path(fp_out, 'QCEW_n_establishments_NAICS_by_size.csv'))
# write_csv(qcew12_n_emp, file.path(fp_out, 'QCEW_n_employees_NAICS_by_size.csv'))
# write_csv(susb_n_firms, file.path(fp_out, 'SUSB_n_firms_NAICS_by_size.csv'))
# write_csv(susb_n_estabs, file.path(fp_out, 'SUSB_n_establishments_NAICS_by_size.csv'))
# write_csv(susb_n_employees, file.path(fp_out, 'SUSB_n_employees_NAICS_by_size.csv'))
# write_csv(susb_n_payroll, file.path(fp_out, 'SUSB_payroll_NAICS_by_size.csv'))
# write_csv(susb_n_rcpts, file.path(fp_out, 'SUSB_receipts_NAICS_by_size.csv'))


# New summary, NAICS only SUSB --------------------------------------------

susb_summ <- susb12_us %>%
  filter(NAICS %in% susb_naics_notredundant) %>%
  mutate(ENTRSIZE = factor(ENTRSIZE, levels = levels(ENTRSIZE)[c(2,3,4,5,1)])) %>%
  arrange(NAICS, ENTRSIZE) %>%
  setNames(c('NAICS', 'NAICS description', 'Size class', 'No. firms', 'No. establishments', 'No. employees', 'Total payroll', 'Total receipts'))

# Remove the non-food codes
food_naics2012 <- unique(bea_naics$related_2012_NAICS_6digit[bea_naics$BEA_Code %in% food_crosswalk$BEA_code])
# Better source of food-only naics codes for the wholesale ones
naics2012classified <- read_csv(file.path(fp_crosswalks, '2012naics_foodclassified.csv'))
food_naics2012_wholesale <- naics2012classified$NAICS12[!is.na(naics2012classified$is_food)]

wholesale_notfood <- grep('^42|^44|^45|^49', food_naics2012, value = TRUE)
wholesale_notfood <- wholesale_notfood[!wholesale_notfood %in% food_naics2012_wholesale]
  
susb_summ <- susb_summ %>%
  filter(NAICS %in% food_naics2012, !NAICS %in% wholesale_notfood)

write_csv(susb_summ, file.path(fp_out, 'SUSB_NAICS_allvariables.csv'))

# Format the summary as the table should appear as excel file (if needed)
