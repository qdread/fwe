# Read Biotime dataset

library(data.table)
library(tidyverse)

biot <- fread('/nfs/qread-data/raw_data/Biotime/BioTIMEQuery02_04_2018.csv')
biotmeta <- fread('/nfs/qread-data/raw_data/Biotime/BioTIMEMetadata_02_04_2018.csv')

# Look at study IDs by time span
length(unique(biot$STUDY_ID))

yearranges <- biot %>%
  group_by(STUDY_ID) %>%
  summarize(begin = min(YEAR), end = max(YEAR)) 

ggplot(yearranges, aes(y = STUDY_ID, yend = STUDY_ID, x = begin, xend = end)) +
  geom_segment()

study_scale <- biot %>%
  group_by(STUDY_ID) %>%
  summarize(lat_range = abs(diff(range(LATITUDE))),
            lon_range = abs(diff(range(LONGITUDE))))

# Look at metadata
biotmeta %>% 
  filter(REALM == 'Terrestrial')
year_loc <- biot %>% 
  group_by(STUDY_ID, LATITUDE, LONGITUDE) %>%
  summarize(begin = min(YEAR), end = max(YEAR))

# Create a box for continental USA


