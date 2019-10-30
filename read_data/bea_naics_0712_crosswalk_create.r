# Create mapping that unites BEA codes, 2007 NAICS codes, and 2012 NAICS codes
# QCR 29 Oct 2019 FWE

# The provided BEA to NAICS mapping is only valid for 2007 but the 2012 codes are different


# Read and tidy the bea to naics07 crosswalk ------------------------------

library(tidyverse)
library(readxl)

fp_crosswalks <- '/nfs/qread-data/crossreference_tables'

bea_naics <- read_xlsx(file.path(fp_crosswalks, 'NAICS/GDPbyInd_GO_NAICS_1997-2016.xlsx'), sheet = 'NAICS codes', skip = 3, na = 'n/a')

# Quickly turn ugly formatting into tidy formatting
bea_naics <- bea_naics %>%
  select(3,4,6) %>%
  setNames(c('BEA_Code', 'BEA_Title', 'related_2007_NAICS')) %>%
  filter(!is.na(BEA_Title))

# Parse string into list of codes, including completing the ones with hyphens
bea_naics <- bea_naics %>%
  mutate(related_2007_NAICS = strsplit(related_2007_NAICS, ', ')) %>%
  unnest(related_2007_NAICS)

hyphen_to_list <- function(x) {
  x_split <- strsplit(x, '-')
  if (length(x_split[[1]]) == 1) return(x)
  # Only needs to accommodate one number after the hyphen
  stem <- substr(x_split[[1]][1], 1, nchar(x_split[[1]][1]) - 1)
  last_numbers <- seq(as.numeric(substr(x_split[[1]][1], nchar(x_split[[1]][1]), nchar(x_split[[1]][1]))), as.numeric(x_split[[1]][2]))
  paste0(stem, last_numbers)
}

bea_naics <- bea_naics %>% 
  mutate(related_2007_NAICS = map(related_2007_NAICS, hyphen_to_list)) %>%
  unnest(related_2007_NAICS)

# combine bea-naics CW with naics 7-12 CW ---------------------------------

naics07to12 <- readxl::read_xlsx(file.path(fp_crosswalks, 'NAICS/2007_to_2012_NAICS.xlsx'), skip = 2) %>%
  select(1:4) %>%
  setNames(c('NAICS07','NAICS07title','NAICS12','NAICS12title'))

# Match shorter NAICS code groupings with the six digit codes in the 2007 to 2012 mapping
short_to_sixdigit_matches <- map(bea_naics$related_2007_NAICS, ~ grep(paste0('^', .), naics07to12$NAICS07))

bea_naics <- bea_naics %>% mutate(related_2007_NAICS_6digit = map(short_to_sixdigit_matches, ~ naics07to12$NAICS07[.]),
                                  related_2012_NAICS_6digit = map(short_to_sixdigit_matches, ~ naics07to12$NAICS12[.]))

bea_naics <- bea_naics %>%
  unnest(c(related_2007_NAICS_6digit, related_2012_NAICS_6digit))

write_csv(bea_naics, file.path(fp_crosswalks, 'BEA_NAICS07_NAICS12_crosswalk.csv'))
