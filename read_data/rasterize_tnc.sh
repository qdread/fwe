# Rasterize the TNC AEA shapefile to 1 km resolution
gdal_rasterize -a ECO_ID_U -tr 1000 1000 /nfs/fwe-data/landuse/ecoregions/tnc_usa_aea.shp /nfs/fwe-data/landuse/ecoregions/tnc_usa_aea_gridded.tif
