# Impute land exchanges for different states
# Part of workflow: Bridge EEIO with FAF to get virtual transfers
# Needed: FAF data, EEIO data (satellite table), crosswalk
# May need to directly load satellite table


# Load FAF data -----------------------------------------------------------

library(tidyverse)
library(sf)

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
fp_cfs <- file.path(fp, 'commodity_flows/CFS')
fp_faf <- file.path(fp, 'commodity_flows/FAF')
fp_satellite <- file.path(fp, 'IO_tables/output_csvs')
fp_crosswalk <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')

faf <- read_csv(file.path(fp_faf, 'FAF4.4.1.csv'), col_types = paste(c(rep('f', 9), rep('n', 31-9)), collapse = ''))


# Load EEIO data ----------------------------------------------------------

# Land exchanges summed up elsewhere (convert location names to underscores)
eeio_land_exch <- read_csv(file.path(fp_satellite, 'land_exchanges_bytype.csv')) %>%
  mutate(Location = gsub('-', '_', Location))

# Load crosswalk table
load(file.path(fp_crosswalk, 'NAICS_BEA_SCTG_crosswalk.RData'))


# Convert EEIO data location to CFS area ----------------------------------

# Crosswalk table for states and CFS regions
fafmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions'), layer = 'faf_aea')
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions'), layer = 'cfs_aea')

# Match ANSI_ST and state names
# assign DC to MD for this analysis.
fips <- read_csv('/nfs/qread-data/state_fips_master.csv')
unique_cfs12 <- unique(with(fafmap, data.frame(CFS12_NAME, ANSI_ST))) %>%
  left_join(fips %>% mutate(ANSI_ST = sprintf('%02d', state)) %>% select(ANSI_ST, state_abbr)) %>%
  mutate(state_abbr = if_else(ANSI_ST == '11', 'MD', state_abbr)) %>%
  mutate(Location = paste('US', state_abbr, sep = '_'))

# Join CFS with EEIO exchanges
cfs_land_exch <- unique_cfs12 %>%
  select(CFS12_NAME, Location) %>%
  full_join(eeio_land_exch)

# Look at what is missing
wide_exch <- eeio_land_exch %>%
  group_by(Landuse_type, Activity_Name, Code) %>%
  spread(Location, Amount)

wide_exch %>% print(n=25)
wide_exch %>% filter(is.na(US)) # Only 10 rows don't have a US wide value.
n_missing <- apply(wide_exch, 1, function(x) sum(is.na(x))) # The majority are almost all so we need to use US wide value except for the ones that have only a few missing

wide_exch_fewmissing <- wide_exch %>% ungroup %>% filter(n_missing < 40) # 13 rows
idx_fewmissing <- which(n_missing < 40)

# Do imputation based on the average values of neighbors, weighted by their sizes
# Read adjacency matrix of states from json
state_adj <- rjson::fromJSON(file = 'https://gist.github.com/Glench/3906059/raw/1a5bfda17eec342ac43873256a1dc1edc2c3c5d1/adjacent_us_states.json')

# Generate a binary matrix from the adjacency list.
state_adjmat <- map(state.abb, ~ state.abb %in% state_adj[[.x]])
state_adjmat <- do.call(rbind, state_adjmat)
dimnames(state_adjmat) <- list(paste0('US_',state.abb), paste0('US_',state.abb))

# Multiply the matrix by the state populations.
library(tidycensus)
census_api_key(readLines('/nfs/qread-data/censusapikey.txt'))
state_pop <- get_decennial(geography = 'state', variables = 'P001001')
state_pop_vec <- with(state_pop, value[match(state.name, NAME)]) # Reorder state population by the state.name vector

state_adjmat_popweights <- sweep(state_adjmat, 1, state_pop_vec, `*`)

# Want to impute based on (1) covariance among values (2) neighborhoods and (3) weight neighbors by population
exchanges_to_impute <- wide_exch_fewmissing %>% select(US_AK:US_WY) %>% t
#exchanges_to_impute <- exchanges_to_impute[dimnames(state_adjmat_popweights)[[1]],]

# Kronecker product
exch_cor <- cor(exchanges_to_impute, use = 'pairwise.complete')
adj_by_cor <- state_adjmat_popweights %x% exch_cor

#---------------------------------------------------
# Impute with just the covariance.
library(mice)
exchanges_imputed <- mice(data = exchanges_to_impute, m = 5, seed = 444)
complete(exchanges_imputed)
# Later can add spatial to this, but we will stick with that at the moment
#---------------------------------------------------

# Fill the missing ones back in to the full dataset

fillin_idx <- which(names(wide_exch_fewmissing) %in% c('US_AK', 'US_WY'))
wide_exch_missingimputed <- wide_exch_fewmissing
wide_exch_missingimputed[, seq(fillin_idx[1], fillin_idx[2])] <- t(complete(exchanges_imputed))

wide_exch_allmissingimputed <- wide_exch
wide_exch_allmissingimputed[idx_fewmissing, ] <- wide_exch_missingimputed

# Use US wide values if no value is given for one of the states
wide_exch_allmissingimputed <- wide_exch_allmissingimputed %>%
  ungroup %>%
  mutate_at(vars(US_AK:US_WY), ~ if_else(is.na(.x), US, .x))

# Some are still NA
wide_exch_allmissingimputed %>% filter(is.na(US), is.na(US_AK)) %>% t

# Replace one row with the US Unspecified number, then get rid of the rows with no data
wide_exch_allmissingimputed[wide_exch_allmissingimputed$Code %in% '111900' & is.na(wide_exch_allmissingimputed$US_AK), which(is.na(wide_exch_allmissingimputed[wide_exch_allmissingimputed$Code %in% '111900' & is.na(wide_exch_allmissingimputed$US_AK),]))] <- wide_exch_allmissingimputed[wide_exch_allmissingimputed$Code %in% '111900' & is.na(wide_exch_allmissingimputed$US_AK), 'US_Unspecified']

wide_exch_allmissingimputed <- wide_exch_allmissingimputed %>% filter(!is.na(US_AK))

# Write the imputed exchange values
write_csv(wide_exch_allmissingimputed, file.path(fp_out, 'land_imputed_exchanges_wide.csv'))
