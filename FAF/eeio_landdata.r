# Exploratory visualizations of EEIO land satellite data
library(tidyverse)
land_sat <- read_xlsx(file.path("/nfs/qread-data/raw_data/IO_tables/USEEIO/USEEIOv1.1SatelliteTables", 'USEEIOv1.1_Satellite_Land.xlsx'), sheet = 'Export')

# Also get the one with some spatial information, fixing names
land_exch <- read_xlsx(file.path("/nfs/qread-data/raw_data/IO_tables/USEEIO/USEEIOv1.1SatelliteTables", 'USEEIOv1.1_Satellite_Land.xlsx'), sheet = 'Exchanges', skip = 4)
names(land_exch)[c(1,2,6,10)] <- c('Exchange_Name','CAS_number','Activity_Name', 'Unit_per_USD_output')
names(land_exch) <- gsub(' ', '_', names(land_exch))

table(land_exch$Location)

ag_exch <- land_exch %>%
  filter(grepl('^11', Code)) 

table(ag_exch$Location, ag_exch$Code)

# What are the multiple ones?
ag_exch %>% filter(Location %in% 'US-AL', Code %in% '111300')

table(land_exch$Unit_per_USD_output) # all in m^2

# Sum up the land amounts per output to get a single value for each location and code

table(land_exch$Exchange_Name)

land_exch_sums <- land_exch %>%
  group_by(Activity_Name, Code, Location) %>%
  summarize(Amount = sum(Amount))

land_exch_sums %>%
  ungroup %>%
  filter(grepl('^11', Code)) %>%
  select(Location, Code) %>%
  table

# Convert the exchanges to the 6 categories in Chaudhary's characterization factors.
lu_types <- c("Annual crops", "Extensive forestry", "Intensive forestry", "Pasture", "Permanent crops", "Urban")
exch_types <- c("Occupation, pasture and meadow, extensive", "Occupation, forest", 
                "Occupation, industrial area", "Occupation, urban, discontinuously built", 
                "Occupation, traffic area, road network", "Occupation, traffic area, rail network", 
                "Occupation, industrial area, built up", "Occupation, annual crop", 
                "Occupation, annual crop, flooded crop", "Occupation, permanent crop, fruit, intensive", 
                "Occupation, agriculture + forest", "Occupation, permanent crop", 
                "Occupation, pasture and meadow", "Occupation, pasture and meadow, intensive", 
                "Occupation, mineral extraction site")

land_exch_sums_bytype <- land_exch %>%
  mutate(Landuse_type= factor(Exchange_Name) %>% fct_collapse('Annual crops' = exch_types[c(8,9)],
                                                              'Extensive forestry' = exch_types[c(2)],
                                                              'Intensive forestry' = exch_types[c(11)],
                                                              'Pasture' = exch_types[c(1, 13, 14)],
                                                              'Permanent crops' = exch_types[c(10, 12)],
                                                              'Urban' = exch_types[c(3,4,5,6,7)],
                                                              'Mining' = exch_types[15])) %>%
  group_by(Landuse_type, Activity_Name, Code, Location) %>%
  summarize(Amount = sum(Amount)) %>%
  ungroup

land_exch_sums_bytype %>%
  filter(grepl('^11', Code)) %>%
  select(Location, Code) %>%
  table

# Some have multiple entries because some codes in some locations contribute to >1 land use type

# Write the modified output so that we can read it in later.
write_csv(land_exch_sums_bytype, file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data/raw_data'), 'IO_tables/output_csvs/land_exchanges_bytype.csv'))
