#################################################################################################################################################################
# Read, Q. D., et al. 2019. Assessing the environmental impacts of halving food loss and waste along the food supply chain. Science of the Total Environment.   #
#                                                                                                                                                               #
# Code to reproduce all analyses in manuscript, beginning with cleaned data                                                                                     #
# Last modified by QDR on 06 January 2020                                                                                                                       #
# Contact: qread@sesync.org                                                                                                                                     #
#################################################################################################################################################################

#########################################  
# Source scripts needed to run analysis #
#########################################

library(tidyverse)
library(reticulate)

# Source R script that is used to build the EEIO model for each scenario

# Source Python script so that the built EEIO models can be called from R

########################
# Load raw data into R #
########################

# Food loss and waste rates by food type and food supply chain stage
flw_rates <- read.csv('flw_rates.csv', stringsAsFactors = FALSE)

# BEA industries from input-output table with the proportion of each industry assigned to the food supply chain,
# and within each industry the proportion assigned to each food commodity group
industry_proportions <- read.csv('industry_proportions.csv', stringsAsFactors = FALSE)

# Make and use tables used to build the EEIO model, with 2012 values mapped to the 2007 BEA schema
M <- read.csv('make2012.csv', row.names = 1, check.names = FALSE)
U <- read.csv('use2012.csv', row.names = 1, check.names = FALSE)

# Table to correct the formatting of the BEA codes
bea_code_formats <- read.csv('industry_codes.csv', stringsAsFactors = FALSE)

#################################################################################################### 
# Calculate 0%, 50% and 100% FLW reduction environmental impact values for each supply chain stage #
####################################################################################################

###########################
# Do uncertainty analysis #
###########################

################################################################################
# Calculate environmental impact reductions for food-specific waste reductions #
################################################################################