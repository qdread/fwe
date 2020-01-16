
library(rgdal)
library(dplyr)

x <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_stops', layer = 'bbsStopsPRISMproj')
out <- x@data %>% select(rteNo, stopNo, POINT_X, POINT_Y) %>% rename(Stop = stopNo, lon = POINT_X, lat = POINT_Y)
write.csv(out, '/mnt/home/qdr/bbs_stop_coords.csv', row.names = FALSE)

# Create text file with a small number of BBS coordinates as a test, then a full one

bbscoords <- read.csv('/nfs/qread-data/raw_data/BBS/bbs_stop_coords.csv')
write.table(bbscoords[1:10,c('lon','lat')], row.names = FALSE, col.names = FALSE, file = '/nfs/qread-data/raw_data/BBS/test.txt')
write.table(bbscoords[,c('lon','lat')], row.names = FALSE, col.names = FALSE, file = '/nfs/qread-data/raw_data/BBS/stopcoords.txt')

# run getnlcd.sh then load the extracted NLCD values and combine them with the coordinates.
bbsstopnlcd <- read.table('/nfs/qread-data/raw_data/BBS/stopnlcd.txt', header = FALSE)

# Dimensions do not match so we need to get the bounding box of the NLCD to see if some are outside the box.
# Turns out they are the Alaskan routes.
library(raster)
n16 <- raster('/nfs/qread-data/raw_data/landuse/NLCD/NLCD_2016_Land_Cover_L48_20190424.img')

n16ex <- extent(n16)
n16box <- t(bbox(n16))
n16box <- SpatialPoints(coords = n16box, proj4string = CRS(projection(n16)))
n16boxlatlon <- spTransform(n16box, CRSobj = CRS('+proj=longlat'))

ex <- extent(n16boxlatlon)
outside_box <- with(bbscoords, (lon < ex@xmin | lon > ex@xmax) & (lat < ex@ymin | lat > ex@ymax))

bbscoords_nlcd <- bbscoords
bbscoords_nlcd$NLCD <- NA
bbscoords_nlcd$NLCD[!outside_box] <- bbsstopnlcd[,1]
write.csv(bbscoords_nlcd, file = '/nfs/qread-data/raw_data/BBS/bbs_stop_coords_nlcd.csv', row.names = FALSE)