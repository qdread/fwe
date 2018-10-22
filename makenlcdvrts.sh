# Create all VRTs for NLCD

# Mosaicked VRT for old land cover
cd /nfs/qread-data/USGS_LC
gdalbuildvrt giras.vrt ./giras*.tif

# VRTs from single large imgs for new land cover
gdalbuildvrt /nfs/qread-data/NLCD/nlcd2006landcover.vrt /nfs/public-data/NLCD/nlcd_2006_landcover_2011_edition_2014_03_31/nlcd_2006_landcover_2011_edition_2014_03_31.img
gdalbuildvrt /nfs/qread-data/NLCD/nlcd2011landcover.vrt /nfs/public-data/NLCD/nlcd_2011_landcover_2011_edition_2014_03_31/nlcd_2011_landcover_2011_edition_2014_03_31.img
gdalbuildvrt /nfs/qread-data/NLCD/nlcd20012006change.vrt /nfs/public-data/NLCD/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09.img
gdalbuildvrt /nfs/qread-data/NLCD/nlcd20062011change.vrt /nfs/public-data/NLCD/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09.img
