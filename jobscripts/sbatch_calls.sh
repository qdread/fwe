cd ~/fwe/jobscripts

# Extract NLCD2016 by combined BCR, FAF, and TNC
sbatch --export=vector_file=/nfs/fwe-data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=/nfs/fwe-data/landuse/NLCD/nlcd2016landcover.vrt,output_file=/nfs/fwe-data/landuse/NLCD/NLCD_2016_BCR.csv countpixels.sh
sbatch --export=vector_file=/nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=/nfs/fwe-data/landuse/NLCD/nlcd2016landcover.vrt,output_file=/nfs/fwe-data/landuse/NLCD/NLCD_2016_FAF.csv countpixels.sh
sbatch --export=vector_file=/nfs/fwe-data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=/nfs/fwe-data/landuse/NLCD/nlcd2016landcover.vrt,output_file=/nfs/fwe-data/landuse/NLCD/NLCD_2016_TNC.csv countpixels.sh

# Use the historic 1700 raster.
sbatch --export=vector_file=/nfs/fwe-data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=/nfs/fwe-data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=/nfs/fwe-data/landuse/historic/BCR1700.csv countpixels.sh
sbatch --export=vector_file=/nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=/nfs/fwe-data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=/nfs/fwe-data/landuse/historic/FAF1700.csv countpixels.sh
sbatch --export=vector_file=/nfs/fwe-data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=/nfs/fwe-data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=/nfs/fwe-data/landuse/historic/TNC1700.csv countpixels.sh

# Extract USDA Cropland Data Layer 2018 for BCR, FAF, and TNC
cdlraster="/nfs/fwe-data/landuse/USDAcropland/CDL/cdl2018.vrt"
outdir="/nfs/fwe-data/landuse/USDAcropland/CDL"
sbatch --export=vector_file=/nfs/fwe-data/landuse/ecoregions/bcr_usa_combined.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_BCR.csv countpixels.sh
sbatch --export=vector_file=/nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_FAF.csv countpixels.sh
sbatch --export=vector_file=/nfs/fwe-data/landuse/ecoregions/tnc_usa_aea.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_TNC.csv countpixels.sh
