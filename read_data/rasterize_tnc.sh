# Rasterize the TNC AEA shapefile to 1 km resolution
gdal_rasterize -a ECO_ID_U -tr 1000 1000 /nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp /nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea_gridded.tif

# Rasterize BCR AEA shapefile to 1 km resolution
gdal_rasterize -a BCR -tr 1000 1000 /nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp /nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_aea_gridded.tif
