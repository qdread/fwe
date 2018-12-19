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


# Use manually created lookup table to match all names --------------------

bbsspp <- read.csv('Q:/BBS/bbs_species_lookup_table_modified.csv', stringsAsFactors = FALSE)
iucnspp <- read.csv('Q:/BBS/iucn_us_bird_spp.csv', stringsAsFactors = FALSE)
bbsfg <- read.csv('Q:/BBS/bbs_fgs.csv', check.names = FALSE)
bbs_iucn_crosswalk <- read.csv('Q:/BBS/bbs_iucn_crosswalk.csv', stringsAsFactors = FALSE)
iucnhabitat <- read.csv('Q:/BBS/iucn_habitat_bbs.csv', stringsAsFactors = FALSE)

library(dplyr)
library(purrr)
library(tidyr)

# Assign species to whether they breed in forests

# major habitat groupings:
habs <- unique(iucnhabitat[,c('code','habitat')])
habs <- habs[order(habs$code), ]
major_habitats <- c(Forest = 1, Shrubland = 3, Artificial_Terrestrial = 14, Savanna = 2, Grassland = 4, Marine_Intertidal = 12, Artificial_Aquatic = 15, Wetland = 5, Marine_Neritic = 9, Marine_Oceanic = 10, Desert = 8, Marine_Coastal = 13, Introduced_Veg = 16, Other = 17, Rocky = 6, Caves = 7)

iucnhabitat <- iucnhabitat %>%
  mutate(major_habitat = names(major_habitats)[match(map_int(strsplit(code, '\\.'), ~ as.integer(.x[1])), major_habitats)])

iucnclasses <- iucnhabitat %>%
  group_by(taxonid, major_habitat) %>%
  summarize(specialist = any(suitability == 'Suitable' & majorimportance == 'Yes'))

iucnclasses_wide <- iucnclasses %>%
  ungroup %>%
  spread(major_habitat, specialist, fill = FALSE)

# Create a few functional groups based on habitats.
# first combine some of the groups
iucnclasses_wide_reduced <- with(iucnclasses_wide, data.frame(taxonid = taxonid,
                                                              artificial = Artificial_Terrestrial | Introduced_Veg,
                                                              forest = Forest,
                                                              grass_shrub = Grassland | Savanna | Shrubland,
                                                              aquatic = Artificial_Aquatic | Marine_Coastal | Marine_Intertidal | Marine_Neritic | Marine_Oceanic | Wetland,
                                                              other = Caves | Desert | Rocky | Other))

# Now define the specialists
iucnclasses_wide_reduced <- iucnclasses_wide_reduced %>%
  mutate(habitat_group = case_when(
    forest & !artificial & !grass_shrub & !aquatic ~ 'forest',
    !forest & !artificial & grass_shrub & !aquatic ~ 'grassland',
    !forest & !artificial & !grass_shrub & aquatic ~ 'aquatic',
    TRUE ~ 'other'
  ))

# Join with the other iucn identifying data
iucnspp <- iucnspp %>%
  left_join(iucnclasses_wide_reduced)

# Join with the bbs data
bbsspp <- bbsspp %>%
  left_join(bbs_iucn_crosswalk %>% rename(Latin_Name_clean = bbs_name))

bbsspp <- bbsspp %>%
  filter(AOU %in% bbsfg$AOU)

# Check and make sure all bbs species are represented somewhere in iucn before joining
name_ok <- with(bbsspp, Latin_Name_clean %in% iucnspp$scientific_name | Latin_Name_synonym %in% iucnspp$scientific_name | Latin_Name_synonym2 %in% iucnspp$scientific_name | iucn_name %in% iucnspp$scientific_name)
                         
table(name_ok) # 3 still missing
bbsspp[!name_ok, ] # These are the three that we don't have in IUCN US species, because they "don't live in the USA."

# We can manually add some stuff for them.
manual_habitat <- data.frame(AOU = c(21930, 52021, 16600), habitat_group = c('aquatic', 'other', 'forest'), stringsAsFactors = FALSE)

# Now join the BBS with the IUCN
bbsspp <- bbsspp %>%
  mutate(habitat_group = case_when(
    iucn_name %in% iucnspp$scientific_name ~ iucnspp$habitat_group[match(iucn_name, iucnspp$scientific_name)],
    Latin_Name_synonym2 %in% iucnspp$scientific_name ~ iucnspp$habitat_group[match(Latin_Name_synonym2, iucnspp$scientific_name)],
    Latin_Name_synonym %in% iucnspp$scientific_name ~ iucnspp$habitat_group[match(Latin_Name_synonym, iucnspp$scientific_name)],
    Latin_Name_clean %in% iucnspp$scientific_name ~ iucnspp$habitat_group[match(Latin_Name_clean, iucnspp$scientific_name)],
    TRUE ~ as.character(NA)
  ))

# Add the species that were not in IUCN to the classification
bbsspp$habitat_group[match(manual_habitat$AOU, bbsspp$AOU)] <- manual_habitat$habitat_group

bbsfg <- bbsfg %>%
  left_join(bbsspp %>% select(AOU, habitat_group))

write.csv(bbsfg, 'Q:/BBS/bbs_fgs_andhabitat.csv', row.names = FALSE)
