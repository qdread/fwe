# Exploratory CBP and SUSB maps
# QDR / FWE / 11 Feb 2019

library(tidyverse)
library(urbnmapr) # Pkg created by Urban Institute
library(reshape2)

ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "oldlace", color = "gray50") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

household_data <- left_join(countydata, counties, by = "county_fips") 

# County population
countypop <- read.csv(file.path(fp, 'Census/co-est2017-alldata.csv'), colClasses = cs[c(rep(1,5), 3,3,rep(2, 132-7))]) %>%
  mutate(county_fips = paste0(as.character(STATE), as.character(COUNTY))) 

# Reduce variables and join with map data
countypop_reduced <- countypop %>%
  select(SUMLEV:POPESTIMATE2017, county_fips) %>%
  left_join(counties, by = "county_fips")



# Choropleth of county population in 2012 for a test
# Log-transformed fill scale
brks <- as.integer(10^(3:8))
logfill <- scale_fill_viridis_c(name = '2012 Population', trans = 'log', breaks = brks, labels = brks)
ez_albers <- coord_map(projection = "albers", lat0 = 39, lat1 = 45)

ggplot() + 
  geom_polygon(data = countypop_reduced, mapping = aes(x = long, y = lat, group = group, fill = POPESTIMATE2012), color = NA) +
  ez_albers +
  logfill +
  ggtitle('Raw 2012 population')

# Percent of population in agriculture
agbycounty <- cbp12co %>% 
  rename(NAICS = naics) %>%
  left_join(naics12) %>%
  filter(NAICS %in% c('11----')) %>%
  mutate(county_fips = paste0(as.character(fipstate), as.character(fipscty)))

# Join with the county map data
agbycounty <- countypop_reduced %>%
  left_join(agbycounty) %>%
  mutate(emp_pct = emp/POPESTIMATE2012)
  
ggplot() + 
  geom_polygon(data = agbycounty, mapping = aes(x = long, y = lat, group = group, fill = emp_pct), color = NA) +
  ez_albers +
  ggtitle('Percent population employed in agriculture, 2012')

ggplot() + 
  geom_polygon(data = agbycounty, mapping = aes(x = long, y = lat, group = group, fill = est), color = NA) +
  ez_albers +
  scale_fill_viridis_c(name = 'Establishments') +
  ggtitle('Number of agricultural establishments, 2012')

# Agricultural crops and livestock.
agbycounty_bycategory <- cbp12co %>% 
  rename(NAICS = naics) %>%
  left_join(naics12) %>%
  filter(substr(NAICS,1,2) == '11') %>%
  mutate(county_fips = paste0(as.character(fipstate), as.character(fipscty))) %>%
  select(county_fips, NAICS, emp) %>%
  group_by(county_fips) %>%
  spread(NAICS, emp, fill = 0) %>%
  mutate(ag = `1151//` + `1152//`)

agbycounty_bycategory_formap <- countypop_reduced %>%
  left_join(agbycounty_bycategory) %>%
  mutate(ag_pct = ag/POPESTIMATE2012)

ggplot() + 
  geom_polygon(data = agbycounty_bycategory_formap, mapping = aes(x = long, y = lat, group = group, fill = ag_pct), color = NA) +
  ez_albers +
  scale_fill_viridis_c(name = 'Pct pop in ag') +
  ggtitle('Percent population employed in agriculture, 2012')

# This seems wrong because most counties now have zeroes.
ggplot() + 
  geom_polygon(data = agbycounty_bycategory_formap, mapping = aes(x = long, y = lat, group = group, fill = ag), color = NA) +
  ez_albers +
  scale_fill_viridis_c(name = 'Employees', trans = 'log', breaks = c(10,30,100,300,1000,3000), labels=c(10,30,100,300,1000,3000)) +
  ggtitle('Number of people employed in agriculture, 2012')
