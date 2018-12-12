### TOKEN FOR ACCESSING IUCN API ###########################################
### This is only authorized to be used by people from MSU ##################
token <- '3b3db1c753a7cad0616e16146363ec88eead97d185cb3304070d7343123516fd'
############################################################################


# Names of all bird species in USA ------------------------------

library(httr)
library(jsonlite)
library(dplyr)

api_url <- 'apiv3.iucnredlist.org/api/v3'

# Get all species in USA
us_spp <- GET(url = paste0(api_url, '/country/getspecies/US?token=', token))
us_spp <- fromJSON(content(us_spp, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

# Get species list of all birds across all countries
all_birds <- GET(url = paste0(api_url, '/comp-group/getspecies/birds?token=', token))
all_birds <- fromJSON(content(all_birds, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

# Subset all bird list by those occurring in USA 
us_birds <- subset(all_birds, taxonid %in% us_spp$taxonid)


# Get habitat for each species --------------------------------------------

# Include error-checking code in the function so that it will return NA for everything if there's no data.

get_habitat <- function(ID) {
  habitat_by_sp <- GET(url = paste0(api_url, '/habitats/species/id/', ID,'?token=', token))
  habitat_by_sp <- fromJSON(content(habitat_by_sp, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result
  if (class(habitat_by_sp) == 'data.frame') {
    data.frame(taxonid=ID, habitat_by_sp)
  } else {
    data.frame(taxonid=ID)
  }
}

# Apply get_habitat to each row of us_birds
all_habitat <- us_birds %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

write.csv(all_habitat, 'Q:/BBS/iucn_habitat_bbs.csv', row.names = FALSE)
write.csv(us_birds, 'Q:/BBS/iucn_us_bird_spp.csv', row.names = FALSE)


# Match IUCN names with BBS names -----------------------------------------

bbsspp <- read.csv('/nfs/qread-data/BBS/bbs_species_lookup_table_modified.csv', stringsAsFactors = FALSE)
iucnspp <- read.csv('/nfs/qread-data/BBS/iucn_us_bird_spp.csv', stringsAsFactors = FALSE)

table(bbsspp$Latin_Name_clean %in% iucnspp$scientific_name | bbsspp$Latin_Name_synonym %in% iucnspp$scientific_name | bbsspp$Latin_Name_synonym2 %in% iucnspp$scientific_name)

nomatch <- !(bbsspp$Latin_Name_clean %in% iucnspp$scientific_name | bbsspp$Latin_Name_synonym %in% iucnspp$scientific_name | bbsspp$Latin_Name_synonym2 %in% iucnspp$scientific_name)
bbsspp$Latin_Name_clean[nomatch] # Many of these are empty values so that's OK

# Load bbs functional group dataframe
bbsfg <- read.csv('/nfs/qread-data/BBS/bbs_fgs.csv', check.names = FALSE)

# Get the species AOUs that actually appear in the "cleaned" dataset
aous <- bbsfg$AOU

bbsnames <- bbsspp$Latin_Name_clean[match(aous, bbsspp$AOU)]
table(bbsnames %in% iucnspp$scientific_name) # Still 65 are missing
nomatchnames <- data.frame(bbs_name = bbsnames[!bbsnames %in% iucnspp$scientific_name])
write.csv(nomatchnames, '/nfs/qread-data/BBS/bbs_iucn_crosswalk.csv', row.names = FALSE)