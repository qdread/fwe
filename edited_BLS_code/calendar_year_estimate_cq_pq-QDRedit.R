################################################################################
#  PROGRAM NAME:  R SAMPLE CODE FOR COMPUTING A SINGLE CALENDAR YEAR WEIGHTED  #
#                 EXPENDITURE FROM THE PUBLIC USE MICRODATA                    #
#                                                                              #
#  WRITTEN BY:    Arcenis Rojas - 06 December 2016                             #
#  VERSION:       R Version 3.3.1                                              #
#                                                                              #
#  This program outlines the procedures for computing calendar-year, weighted  #
#  expenditure means using the Consumer Expenditure (CE) Interview Survey      #
#  Public Use Microdata (PUMD).                                                #
################################################################################

# Clear the workspace
#rm(list = ls())

################################## User inputs #################################

# File paths updated by QDR, 1 Feb 2019

# Store the year for which an estimate will be calculated
year <- 2016

# Set the path of the folder containing all 5 quarterly FMLI files
data_dir <- paste0("/nfs/fwe-data/CEX/data", substr(year,3,4))

################################ End user inputs ###############################



# Load required packages: data.table and dplyr
library(data.table)
library(dplyr)

# Store a vector of all the file paths to the individual FMLI files
file_paths <- dir(data_dir, pattern = "^fmli.*[.]csv$", full.names = TRUE)

# Read all 5 FMLI files into a list keeping only the necessary variables
mn_exp <- lapply(file_paths, fread,
                 select = c("NEWID", "FINLWT21", "QINTRVMO", "QINTRVYR",
                            "TOTEXPPQ", "TOTEXPCQ")) %>%

    # Stack the five datasets into one large dataset
    bind_rows() %>%

    # Change all variable names to lower case
    setnames(old = names(.), new = tolower(names(.))) %>%

    # QINTRVMO is saved as a character... change it to a numeric variable
    mutate(qintrvmo = as.numeric(qintrvmo)) %>%

    # Add a variable indicating the quarter (1 - 5) based on the interview
    # year and month and one to store a consumer unit's weight divided by four
    # because the sum of FINLWT21 represents the entire U.S. population each
    # quarter ("popweight").
    mutate(
        qtr = ifelse(
            qintrvmo %in% c(1:3) & qintrvyr == year, 1,
            ifelse(qintrvyr %in% (year + 1), 5, 3)
        ),
        popwt = finlwt21 / 4
    ) %>%

    # Calculate total expenditures from the previous and current quarter values
    # depending on which quarter the data are from and generate a variable that
    # will be used to adjust for a given interview's months in scope.
    mutate(
        totexp = ifelse(
            qtr %in% 1, totexpcq,
            ifelse(
                qtr %in% 5, totexppq,
                totexppq + totexpcq
            )
        ),
        mo_scope = (ifelse(
            qtr %in% 1, qintrvmo - 1,
            ifelse(
                qtr %in% 5, 4 - qintrvmo,
                3
            )
        )) / 3
    ) %>%

    # Calculate the "calendar weight" by multiplying the consumer unit's
    # adjusted population weight by its months in scope.
    mutate(calwt = popwt * mo_scope) %>%

    # Divide each consumer unit's weighted expenditure by the sum of
    # calendar-adjusted population weights
    mutate(calwt_exp = (finlwt21 * totexp) / (sum(calwt))) %>%

    # Take the sum of all of the values calculated in the previous step
    summarise(mn_exp = sum(calwt_exp)) %>%

    # Convert the estimate into a numerical value rather than a dataset
    unlist()

# Print the mean estimate to the console
print(mn_exp)
