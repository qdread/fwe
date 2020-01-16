# Extract projection from NLCD raster to prj file, then use it to transform the FAF shapefile to that projection (and the combined CFS shapefile too)
# Also transform the TNC USA shapefile
# Also transform the USA state level shapefile to albers

# Must remove trailing spaces in projections with xargs

# Transform FAF to NLCD projection (AEA)
nlcdproj=`gdalsrsinfo /nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt -o proj4 | xargs`

ogr2ogr -f "ESRI Shapefile" -t_srs "${nlcdproj}" -s_srs EPSG:4326 /nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp /nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/Freight_Analysis_Framework_Regions.shp
ogr2ogr -f "ESRI Shapefile" -t_srs "${nlcdproj}" -s_srs EPSG:4326 /nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/cfs_aea.shp /nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/cfs12.shp

# Transform TNC to AEA projection
tncproj=`gdalsrsinfo /nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa.prj -o proj4 | xargs`

ogr2ogr -f "ESRI Shapefile" -t_srs "${nlcdproj}" -s_srs "${tncproj}" /nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp /nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa.shp

# Transform USA state shapefile (unprojected, wgs84) to AEA projection
usaproj=`gdalsrsinfo /nfs/public-data/GADM/USA_adm/USA_adm1.prj -o proj4 | xargs`
mkdir /nfs/qread-data/raw_data/landuse/USA/
ogr2ogr -f "ESRI Shapefile" -t_srs "${nlcdproj}" -s_srs "${usaproj}" /nfs/qread-data/raw_data/landuse/USA/USA_adm1_aea.shp /nfs/public-data/GADM/USA_adm/USA_adm1.shp
