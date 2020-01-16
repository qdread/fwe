# Attempt to create integrated mean and SE by region and/or state instead of by income group, or by both.
# From BLS CEX data
# QDR 04 Feb 2019

################################################################################
#                                                                              #
#                  Read in and stack the FMLI and FMLD files                   #
#                                                                              #
################################################################################

# Store a vector of replicate weight variable names
wtrep_vars <- paste0("wtrep", str_pad(1:44, 2, "left", 0))

# Read in and stack the fmli files
fmli <- lapply(
  dir(data_dir, pattern = "^fmli.*[.]csv$", full.names = TRUE),
  fread,
  select = c(
    "NEWID", "FINLWT21", toupper(wtrep_vars), "QINTRVMO", "QINTRVYR",
    "INCLASS", "REGION", "STATE"
  ),
  na.strings = c("", ".", "NA")
) %>% bind_rows() %>%
  setnames(old = names(.), new = tolower(names(.))) %>%
  mutate(
    newid = as.character(newid),
    qintrvmo = as.numeric(qintrvmo),
    
    # Generate a calendar-year population weight variable
    popwt = ifelse(
      qintrvmo %in% 1:3 & qintrvyr %in% year,
      (qintrvmo - 1) / 3 * finlwt21 / 4,
      ifelse(
        qintrvyr %in% (year + 1),
        (4 - qintrvmo) / 3 *finlwt21 / 4,
        finlwt21 / 4
      )
    )
  ) %>%
  mutate_at(wtrep_vars, funs(replace(., is.na(.), 0))) %>%
  select(-c(qintrvyr, qintrvmo))

# Read in and stack the fmld files
fmld <- lapply(
  dir(data_dir, pattern = "^fmld.*[.]csv$", full.names = TRUE),
  fread,
  select = c("NEWID", "FINLWT21", toupper(wtrep_vars), "INCLASS", "REGION", "STATE"),
  na.strings = c("", ".", "NA")
) %>% bind_rows() %>%
  setnames(old = names(.), new = tolower(names(.))) %>%
  mutate(newid = as.character(newid), popwt = finlwt21 / 4) %>%
  mutate_at(wtrep_vars, funs(replace(., is.na(.), 0)))


################################################################################
#                                                                              #
#                    Generate expenditure estimate tables                      #
#                                                                              #
################################################################################

# Generate a table of expenditure estimates and standard errors for all
# consumer units
all_cus_bystate <- bind_rows(
  
  # Divide all weight variables by the sum of the calendar year-population
  # variable in the FMLI and FMLD files
  fmli %>% adjWt(),
  fmld %>% adjWt()
) %>%
  
  # Join the expenditure variables to the FMLI and FMLD tables
  left_join(expend, by = "newid") %>%
  
  # Remove all rows with missing expenditure values
  filter(!is.na(cost)) %>%
  
  # Multiply all weight variables by all expenditures
  mutate_at(c("finlwt21", wtrep_vars), funs(. * cost)) %>%
  
  # Group by UCC
  group_by(ucc) %>%
  
  # For each UCC get the sum of the expenditure adjusted weight variables
  summarise_at(c("finlwt21", wtrep_vars), funs(sum(.))) %>%
  ungroup() %>%
  
  # Change the name of the FINLWT21 column to reflect that its values now
  # represent the estimated mean expenditures for each UCC
  setnames(old = "finlwt21", new = "mn") %>%
  
  # Feed the above table into the "makeGM_SE_Table" functiond described above
  makeGM_SE_Table(stub) %>%
  bind_rows %>%
  
  # Merge the resulting table with the stub file
  left_join(stub, ., by = c("title", "rownum")) %>%
  
  # Round mean and standard error values to two decimal places
  mutate_at(vars(mn, se), funs(round(., 2))) %>%
  
  # Keep only necessary variables
  select(title, mn, se, group, rownum) %>%
  
  # Gather the grand mean and standard error value columns into one column
  gather(estimate, value, mn, se) %>%
  
  # Remove standard error values for CU characteristics
  filter(!(group %in% "CUCHARS" & estimate %in% "se")) %>%
  
  mutate(rownum = as.numeric(rownum)) %>%
  arrange(rownum, estimate) %>%
  setnames("value", "All CU's")

# Add a value to the "Number of consumer units" row
all_cus_bystate[all_cus_bystate$title == "Number of consumer units", "All CU's"] <-
  sum(fmli$popwt)

# The "by_row" table will produce the same results as the "all_cus" table
# generated above using the same methods. Below are commented only the code
# that differs from the above procedure.
by_group_region <- bind_rows(
  fmli %>% adjWt("region"),
  fmld %>% adjWt("region")
) %>%
  left_join(expend, by = "newid") %>%
  mutate_at(c("finlwt21", wtrep_vars), funs(. * cost)) %>%
  filter(!is.na(cost)) %>%
  
  # Group by both the grouping variable (region) and UCC
  group_by(region, ucc) %>%
  summarise_at(c("finlwt21", wtrep_vars), funs(sum(.))) %>%
  ungroup() %>%
  setnames(old = "finlwt21", new = "mn") %>%
  
  # Feed the above table into the "makeGM_SE_Table" functiond described above
  # by group
  group_by(region) %>%
  makeGM_SE_Table(stub) %>%
  bind_rows %>%
  
  left_join(stub, ., by = c("title", "rownum")) %>%
  mutate_at(vars(mn, se), funs(round(., 2))) %>%
  select(region, title, group, rownum, mn, se) %>%
  gather(estimate, value, mn, se) %>%
  filter(!(group %in% "CUCHARS" & estimate %in% "se")) %>%
  mutate(rownum = as.numeric(rownum)) %>%
  
  
  
  # Spread the estimates across by region group
  spread(region, value) %>%
  
  # Change the names of the groups to be more legible
  #setnames(old = paste0("0", 1:9), new = paste0("Inclass0", 1:9)) %>%
  # Debug added by QDR 04 Feb 2019
  setnames(old = as.character(1:4), new = paste0("Region0", 1:4)) %>%
  
  arrange(rownum, estimate) %>%
  select(title, estimate, Region01:Region04, rownum, group) %>%
  mutate_at(paste0("Region0", 1:4), funs(replace(., is.na(.), 0)))

by_group_region[
  by_group_region$title == "Number of consumer units",
  paste0("Region0", 1:4)
  ] <- tapply(fmli$popwt, fmli$region, sum)

# Merge the All CU's table with the table by groups
out <- left_join(
  all_cus_bystate, by_group_region,
  by = c("rownum", "title", "estimate", "group")
) %>%
  select(-rownum)

# Remove meaningless estimates
out[
  out$title %in%
    c("Consumer unit characteristics:", "Percent distribution:",
      "Sources of income and personal taxes:", "Addenda:",
      "Other financial information:"),
  c("group", "estimate", "All CU's", paste0("Region0", 1:4))
  ] <- NA

# Remove duplicates
out <- out[!(duplicated(out) |
               out$title %in% c("Consumer unit characteristics:",
                                "Percent distribution:",
                                "Sources of income and personal taxes:",
                                "Addenda:",
                                "Other financial information:")), ]

# Rename Region code to region name.
names(out)[grep('Region', names(out))] <- c('Northeast', 'Midwest', 'South', 'West')

write.csv(out, paste0("/nfs/qread-data/raw_data/CEX/csv_output/Integrated_mean_se_", year, "_byregion.csv"), row.names = FALSE, na = "")
