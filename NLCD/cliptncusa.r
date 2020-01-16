# Create TNC clipped to USA

# Read TNC and USA shapefiles
library(rgdal)
library(rgeos)
tnc <- readOGR(dsn = '/nfs/qread-data/raw_data/landuse/ecoregions', layer = 'tnc_terr_ecoregions')
usa <- readOGR(dsn = '/nfs/public-data/GADM/USA_adm', layer = 'USA_adm0')

# Ensure row names can be matched between polygons and data after the clipping
tnc@data <- tnc@data %>%
  mutate(id = as.character(1:nrow(tnc@data)), region = as.character(ECODE_NAME))

# Do intersection (clip TNC to USA)
tnc_usa <- gIntersection(tnc, usa, byid = TRUE, id = tnc$id)

# Recreate dataframe and add back in to the intersected spatialpolygons
tncdat <- tnc@data
tncdat <- tncdat[as.numeric(names(tnc_usa)),]
row.names(tncdat) <- tncdat$id
tnc_usa <- SpatialPolygonsDataFrame(tnc_usa, tncdat)

# Test to make sure it worked OK
plot(tnc_usa[c("182","183","184"),]) # Looks OK

# Write as shape file
writeOGR(tnc_usa, dsn = '/nfs/qread-data/raw_data/landuse/ecoregions', layer = 'tnc_usa', driver = 'ESRI Shapefile')
