# Script to read US Economic Census data into R and process
# QDR / FWE / 11 Feb 2019

library(tidyverse)

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')

# SUSB: 6 digit NAICS by state for 2012
# Ensure that leading zeroes are preserved in the codes
cs <- c('factor','numeric','character')
susb12 <- read.csv(file.path(fp, 'Census/SUSB/us_state_6digitnaics_2012.txt'), colClasses = cs[c(1,1,1,2,2,2,1,1,2,1,2,1,3,3,3)]) 

# with(susb12, unique(cbind(ENTRSIZE,ENTRSIZEDSCR)))
# with(susb12, unique(cbind(STATE,STATEDSCR)))
# with(susb12, table(ESTB >= FIRM))
# with(susb12, table(ESTB > FIRM))
# with(susb12, table(EMPLFL_R)) # Most have no flag, most flags are a small number of exclusions.
# with(susb12, table(EMPLFL_N))
# with(susb12, table(PAYRFL_N))

# CBP: aggregated by county, MSA, state, and zip code
# By county for 2012
cbp12co <- read.csv(file.path(fp, 'Census/CBP/cbp12co.txt'), colClasses = cs[c(rep(1,5), 2,1,2,1,rep(2,length(10:24)),1,1)])

# Load NAICS reference to see if we can classify by number of digits
naics12 <- read.csv(file.path(fp, 'Census/reference_docs/naics2012.txt'), stringsAsFactors = FALSE)

naics12 <- naics12 %>%
  mutate(ndigits = nchar(gsub('[^0-9]', '', NAICS)))
