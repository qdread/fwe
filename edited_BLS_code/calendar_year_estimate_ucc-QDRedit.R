################################################################################
#  PROGRAM NAME:  R SAMPLE CODE FOR COMPUTING A SINGLE CALENDAR YEAR WEIGHTED  #
#                 EXPENDITURE FROM THE PUBLIC USE MICRODATA USING A UCC        #
#                                                                              #
#  WRITTEN BY:    Arcenis Rojas - 29 AUGUST 2017                               #
#  VERSION:       R Version 3.3.3                                              #
#                                                                              #
#  This program outlines the procedures for computing calendar-year, weighted  #
#  expenditure means using the Consumer Expenditure (CE) Interview Survey      #
#  Public Use Microdata (PUMD).                                                #
################################################################################

library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)
library(readr)

#rm(list = ls())

year <- 2017
ucc_exp <- "WATER"

# Store the directory containing the data files. It is assumed that the FMLI,
# MTBI, and ITBI files are all in the same folder
# File path updated by QDR, 1 Feb 2019
data_dir <- paste0("/nfs/qread-data/raw_data/CEX/data", substr(year,3,4))


################################################################################
#                                                                              #
#          Read in and stack the monthly expenditure and income files          #
#                                                                              #
################################################################################

# Read in and stack the MTBI files
mtbi <- lapply(
    dir(data_dir, pattern = "^mtbi.*[.]csv$",full.names = TRUE),
    fread,
    select = c("NEWID", "COST", "UCC", "REF_YR"),
    na.strings = c("", ".", "NA")
) %>% bind_rows() %>%

    # Change the column names to lower case
    setnames(old = names(.), new = tolower(names(.))) %>%

    # Filter for expenditures made in the given year and UCC's used for
    # publication
    filter(ref_yr %in% year, ucc %in% ucc_exp) %>%

    # Change "newid" to a character variable
    mutate(newid = as.character(newid)) %>%

    # Remove unnecessary columns
    select(-ref_yr) %>%

    # Group the data by newid and UCC
    group_by(newid) %>%

    # Get the sum of expenditures on each UCC for each newid
    summarise(cost = sum(cost))


################################################################################
#                                                                              #
#                       Read in and stack the FMLI files                       #
#                                                                              #
################################################################################

# Read in and stack the fmli files
fmli <- lapply(
    dir(data_dir, pattern = "^fmli.*[.]csv$", full.names = TRUE),
    fread,
    select = c("NEWID", "FINLWT21", "QINTRVMO", "QINTRVYR"),
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
    select(-c(qintrvyr, qintrvmo))


mean_exp <- left_join(fmli, mtbi, by = "newid") %>%
    mutate(cost = replace(cost, is.na(cost), 0)) %>%
    summarise(mean_exp = sum(cost * finlwt21) / sum(popwt)) %>%
    unlist
