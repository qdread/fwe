# Extract projection from NLCD raster to prj file, then use it to transform the FAF shapefile to that projection

# gdalsrsinfo /nfs/fwe-data/landuse/NLCD/nlcd2016landcover.vrt > /nfs/fwe-data/landuse/NLCD/nlcd.prj
# gdalwarp -t_srs "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" /nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/Freight_Analysis_Framework_Regions.shp /nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp

ogr2ogr -f "ESRI Shapefile" -t_srs "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" -s_srs EPSG:4326 /nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp /nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/Freight_Analysis_Framework_Regions.shp

