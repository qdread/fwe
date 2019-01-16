# Map of FAF or CFS regions
# QDR / FWE / 14 Jan 2019

library(rgdal)
library(rgeos)
fafshp <- readOGR(dsn = 'Q:/FAF/Freight_Analysis_Framework_Regions', layer = 'Freight_Analysis_Framework_Regions') # big, need to make smaller version.

# Dissolve by CFS12_NAME
# Also preserve the numeric ID.
faf132 <- gUnaryUnion(fafshp, id = fafshp$CFS12_NAME)
polygonids <- getSpPPolygonsIDSlots(faf132)
polygonids <- data.frame(CFS12_NAME = polygonids)
dimnames(polygonids)[[1]] <- polygonids$CFS12_NAME
faf132spdf <- SpatialPolygonsDataFrame(faf132, data = polygonids)

plot(faf132spdf)
spplot(faf132spdf, xlim = c(-127, -65), ylim = c(25, 50), colorkey = FALSE)
proj4string(faf132spdf)

writeOGR(faf132spdf, dsn = 'Q:/FAF/Freight_Analysis_Framework_Regions', layer = 'cfs12', driver = 'ESRI Shapefile')
