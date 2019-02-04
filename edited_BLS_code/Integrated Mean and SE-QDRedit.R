################################################################################
################################################################################
# Program Name: CE Integrated Surveys Sample Program (R)                       #
# Purpose: Create an integrated survey expenditure table by income class       #
#          using microdata from the Bureau of Labor Statistics' Consumer       #
#          Expenditure Survey.                                                 #
#                                                                              #
# Written By: Arcenis Rojas - 28 August 2017                                   #
#                                                                              #
# Note: Written in R Version 3.3.3                                             #
#                                                                              #
################################################################################
################################################################################

library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)
library(readr)

rm(list = ls())

year <- 2017

# File paths updated by QDR 04 Feb 2019.
# Store the directory containing the data files. It is assumed that the FMLI,
# FMLD, MTBI, EXPD, and ITBI files are all in the same folder
data_dir <- paste0("/nfs/fwe-data/CEX/data", substr(year,3,4))

# Store the path of the integrated stub file
stub_path <- file.path(data_dir, "csxintstub.txt")

# Store the path to which you would like to store the final table
out_path <- paste0("/nfs/fwe-data/CEX/CEX sample R output/Integrated_mean_se_", year, ".csv")


################################################################################
#                                                                              #
#                          Declare helper functions                            #
#                                                                              #
################################################################################

# Function that collects the UCC's for a given expenditure from the stub file
getUCCs <- function(expn, sf, lvl, grp) {
    start_row <- which(sf$title %in% expn & sf$level %in% lvl &
                           sf$group %in% grp)
    start_level <- sf$level[start_row]
    end_row <- if(start_row == nrow(sf)) {
        start_row
    } else {
        min(
            match(start_level, sf$level[(start_row + 1):nrow(sf)]) +
                start_row - 1,
            which(
                sf$level[(start_row + 1):length(sf$level)] < start_level
            )[1] +
                as.numeric(start_row) - 1,
            nrow(sf),
            na.rm = T
        )
    }
    uccs <- sf$ucc[start_row:end_row]
    uccs <- suppressWarnings(uccs[!is.na(as.numeric(uccs))])
    return(uccs)
}


adjWt <- function(df, grp = NULL) {
    if(!is.null(grp)) df %<>% group_by_(grp)
    df %<>% mutate_at(c("finlwt21", wtrep_vars), funs(. / sum(popwt)))
}


# Make a table of means and standard errors based on the stub file
makeGM_SE_Table <- function(df, sf) {

    #Iterate over each row of the stub file
    apply(sf, 1, function(x) {

        # Take in the data table
        df %>%

            # Filter the expenditure table for UCC's associated with the given
            # title
            filter(
                ucc %in% getUCCs(x["title"], stub, x["level"], x["group"])
            ) %>%

            # Add up the vectors of means and replicate differences from the
            # mean
            summarise_at(c("mn", wtrep_vars), funs(sum(.))) %>%

            # Compute the squared differences from the mean
            mutate_at(c(wtrep_vars), funs((. - mn) ^ 2)) %>%

            # Add 3 variables:
            mutate(

                # The standard error
                se = ifelse(
                    !x["group"] %in% "CUCHARS",
                    sqrt(rowSums(.[, wtrep_vars]) / 44), NA
                ),

                # The title
                title = x["title"],

                # And a row number
                rownum = x["rownum"]
            )
    })
}


################################################################################
#                                                                              #
#                       Read in and clean the stub file                        #
#                                                                              #
################################################################################

# Create a temporary file on the local disk..
tf <- tempfile()

# Read the stub file into memory
st <- readLines(stub_path)

# Remove all the lines before the row with the title "Number of consumer
# units"
st <- st[grep("Number of consumer units", st): length(st)]

# Replace these two tabs with seven spaces instead
st <- gsub("\t" , "   " , st)

# Keep only the rows which start with type "*", "1", "2" and with the
# 4th character as a blank space or a digit from 1 through 9
st <- st[substr(st, 1, 1) %in% c("*", 1, 2) &
             substr(st, 4, 5) %in% paste0(c(" ", 1:9), " ")]

# save to the temporary file created above
writeLines(st , tf)

# Read in the cleaner version of the stub file in fixed-width format
stub <- read_fwf(
    file = tf,
    fwf_empty(
        tf, n = 1000L,
        col_names = c("type", "level", "title", "UCC", "survey", "factor",
                      "group")
    )
) %>% select(-factor)

# Concatenate the titles that run beyond 1 line into their respective first
# lines
for (i in seq(length(stub$type))) {
    if (stub$type[i] %in% "2") {
        l1_row <- max(which(stub$type[1:i] %in% "1"))
        stub$title[l1_row] <- paste(stub$title[l1_row], stub$title[i])
    }
}

# Make the names of the stub file columns lower case
stub %<>%
    setnames(old = names(.), new = tolower(names(.))) %>%

    # Filter for rows of type "1" or "*" and filter out unnecessary rows
    filter(
        type %in% c("1", "*"),
        !title %in%
            c("Percent distribution of consumer units", "Lower limit",
              grep("weight", .$title, value = T))
    ) %>%

    # Add a row number column
    mutate(rownum = as.character(seq(nrow(.))))

# Change the title of the row where the number of consumer units will be shown
stub$title[stub$title == "Number of consumer units (in thousands)"] <-
    "Number of consumer units"


write.csv(stub, file.path(data_dir, 'csxistub_cleaned.csv'), row.names = FALSE)

################################################################################
#                                                                              #
#          Read in and stack the monthly expenditure and income files          #
#                                                                              #
################################################################################

# Read in and stack the MTBI files
mtbi <- lapply(
    dir(data_dir, pattern = "^mtbi.*[.]csv$",full.names = TRUE),
    fread,
    select = c("NEWID", "COST", "UCC", "REF_YR", "PUBFLAG"),
    na.strings = c("", ".", "NA")
) %>% bind_rows() %>%

    # Change the column names to lower case
    setnames(old = names(.), new = tolower(names(.))) %>%

    # Filter for expenditures made in the given year and UCC's used for
    # publication
    filter(ref_yr %in% year, pubflag %in% "2") %>%

    # Change "newid" to a character variable
    mutate(newid = as.character(newid)) %>%

    # Remove unnecessary columns
    select(-ref_yr) %>%

    # Group the data by newid and UCC
    group_by(newid, ucc) %>%

    # Get the sum of expenditures on each UCC for each newid
    summarise(cost = sum(cost))


# Read in and stack the ITBI files
itbi <- lapply(
    dir(data_dir, pattern = "^itbi.*[.]csv$", full.names = TRUE),
    fread,
    select = c("NEWID", "VALUE", "UCC", "REFYR", "PUBFLAG"),
    na.strings = c("", ".", "NA")
) %>% bind_rows() %>%
    setnames(old = names(.), new = tolower(names(.))) %>%
    filter(refyr %in% year, pubflag %in% "2") %>%
    mutate(newid = as.character(newid)) %>%
    rename(cost = value) %>%
    select(-refyr) %>% group_by(newid, ucc) %>%
    summarise(cost = sum(cost))

# Read in and stack the expd files
expd <- lapply(
    dir(data_dir, pattern = "^expd.*[.]csv$", full.names = TRUE),
    fread,
    select = c("NEWID", "COST", "UCC", "PUB_FLAG"),
    na.strings = c("", ".", "NA")
) %>% bind_rows() %>%
    setnames(old = names(.), new = tolower(names(.))) %>%
    filter(pub_flag %in% "2") %>%
    mutate(
        newid = as.character(newid),

        # Multiply expenditures by 13 to convert them from weekly to quarterly
        # figures
        cost = cost * 13
    ) %>% group_by(newid, ucc) %>%
    summarise(cost = sum(cost))

# Stack the mtbi and expd files
expend <- bind_rows(mtbi, itbi, expd)

# Remove monthly expenditure and income files from memory
rm(mtbi, expd, itbi)
gc()


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
        "INCLASS"
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
    select = c("NEWID", "FINLWT21", toupper(wtrep_vars), "INCLASS"),
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
all_cus <- bind_rows(

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
all_cus[all_cus$title == "Number of consumer units", "All CU's"] <-
    sum(fmli$popwt)

# The "by_row" table will produce the same results as the "all_cus" table
# generated above using the same methods. Below are commented only the code
# that differs from the above procedure.
by_group <- bind_rows(
    fmli %>% adjWt("inclass"),
    fmld %>% adjWt("inclass")
) %>%
    left_join(expend, by = "newid") %>%
    mutate_at(c("finlwt21", wtrep_vars), funs(. * cost)) %>%
    filter(!is.na(cost)) %>%

    # Group by both the grouping variable (INCLASS) and UCC
    group_by(inclass, ucc) %>%
    summarise_at(c("finlwt21", wtrep_vars), funs(sum(.))) %>%
    ungroup() %>%
    setnames(old = "finlwt21", new = "mn") %>%

    # Feed the above table into the "makeGM_SE_Table" functiond described above
    # by group
    group_by(inclass) %>%
    makeGM_SE_Table(stub) %>%
    bind_rows %>%

    left_join(stub, ., by = c("title", "rownum")) %>%
    mutate_at(vars(mn, se), funs(round(., 2))) %>%
    select(inclass, title, group, rownum, mn, se) %>%
    gather(estimate, value, mn, se) %>%
    filter(!(group %in% "CUCHARS" & estimate %in% "se")) %>%
    mutate(rownum = as.numeric(rownum)) %>%
    
    
  
    # Spread the estimates across by INCLASS group
    spread(inclass, value) %>%

    # Change the names of the groups to be more legible
    #setnames(old = paste0("0", 1:9), new = paste0("Inclass0", 1:9)) %>%
    # Debug added by QDR 04 Feb 2019
    setnames(old = as.character(1:9), new = paste0("Inclass0", 1:9)) %>%

    arrange(rownum, estimate) %>%
    select(title, estimate, Inclass01:Inclass09, rownum, group) %>%
    mutate_at(paste0("Inclass0", 1:9), funs(replace(., is.na(.), 0)))

by_group[
    by_group$title == "Number of consumer units",
    paste0("Inclass0", 1:9)
    ] <- tapply(fmli$popwt, fmli$inclass, sum)

# Merge the All CU's table with the table by groups
out <- left_join(
    all_cus, by_group,
    by = c("rownum", "title", "estimate", "group")
) %>%
    select(-rownum)

# Remove meaningless estimates
out[
    out$title %in%
        c("Consumer unit characteristics:", "Percent distribution:",
          "Sources of income and personal taxes:", "Addenda:",
          "Other financial information:"),
    c("group", "estimate", "All CU's", paste0("Inclass0", 1:9))
    ] <- NA

# Remove duplicates
out <- out[!(duplicated(out) |
                 out$title %in% c("Consumer unit characteristics:",
                                  "Percent distribution:",
                                  "Sources of income and personal taxes:",
                                  "Addenda:",
                                  "Other financial information:")), ]

write.csv(out, out_path, row.names = FALSE, na = "")
