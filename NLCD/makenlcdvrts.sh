# Create all VRTs for NLCD

# Mosaicked VRT for old land cover
cd /nfs/qread-data/USGS_LC
gdalbuildvrt giras.vrt ./giras*.tif

# VRTs from single large imgs for new land cover
gdalbuildvrt /nfs/qread-data/NLCD/nlcd2006landcover.vrt /nfs/public-data/NLCD/nlcd_2006_landcover_2011_edition_2014_03_31/nlcd_2006_landcover_2011_edition_2014_03_31.img
gdalbuildvrt /nfs/qread-data/NLCD/nlcd2011landcover.vrt /nfs/public-data/NLCD/nlcd_2011_landcover_2011_edition_2014_03_31/nlcd_2011_landcover_2011_edition_2014_03_31.img
gdalbuildvrt /nfs/qread-data/NLCD/nlcd20012006change.vrt /nfs/public-data/NLCD/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2001_to_2006_landcover_fromto_change_index_2011_edition_2014_04_09.img
gdalbuildvrt /nfs/qread-data/NLCD/nlcd20062011change.vrt /nfs/public-data/NLCD/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09/nlcd_2006_to_2011_landcover_fromto_change_index_2011_edition_2014_04_09.img

# Edit: 2016 VRT (also hosted on public data)
gdalbuildvrt /nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt /nfs/public-data/NLCD/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img

# Create VRTs for all the USDA cropland data layers that are now on public-data
for ((y=2008; y <= 2018 ; y++))
do
   gdalbuildvrt /nfs/qread-data/raw_data/landuse/USDAcropland/CDL/cdl${y}.vrt /nfs/public-data/USDA/NASS-CDL/${y}_30m_cdls.img
done
