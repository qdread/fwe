# Load Chaudhary data to see the characterization factors of the different ecoregions in the USA

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')

chaud <- read.csv(file.path(fp, 'biodiversity/chaudhary2015SI/chaud2015SI2.csv'), stringsAsFactors = FALSE)

# Load map of TNC ecoregions in USA
library(sf)
library(tidyverse)
tnc_usa <- st_read(dsn = file.path(fp, 'landuse/ecoregions'), layer = 'tnc_usa_aea')

chaud_uniq_region <- unique(chaud$ecoregion)
usa_uniq_region <- as.character(unique(tnc_usa$ECO_CODE))

usa_uniq_region[!usa_uniq_region %in% chaud_uniq_region]

# Map showing which are not in there

tnc_usa <- tnc_usa %>%
  mutate(in_chaud = ECO_CODE %in% chaud_uniq_region)
plot(tnc_usa['in_chaud'])
# 9 regions are not in Chaudhary, and they are distributed more or less randomly around the USA.

# Make a map of the bird characterization factors, trying the occ marginal global 
chaud_birds_global <- chaud %>%
  filter(taxon %in% 'Birds', CF %in% 'Occ_marginal_global') %>%
  select(ecoregion, landuse, median) %>%
  mutate(median = log10(median)) %>%
  spread(landuse, median)

tnc_usa_birds <- tnc_usa %>%
  left_join(chaud_birds_global, by = c('ECO_CODE' = 'ecoregion'))

plot(tnc_usa_birds[unique(chaud$landuse)])
