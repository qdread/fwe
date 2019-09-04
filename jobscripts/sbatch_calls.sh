cd ~/fwe/jobscripts

# Extract NLCD2016 by combined BCR, FAF, and TNC
sbatch --export=vector_file=/nfs/fwe-data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=/nfs/fwe-data/landuse/NLCD/nlcd2016landcover.vrt,output_file=/nfs/fwe-data/landuse/NLCD/NLCD_2016_BCR.csv countpixels.sh
sbatch --export=vector_file=/nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=/nfs/fwe-data/landuse/NLCD/nlcd2016landcover.vrt,output_file=/nfs/fwe-data/landuse/NLCD/NLCD_2016_FAF.csv countpixels.sh
sbatch --export=vector_file=/nfs/fwe-data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=/nfs/fwe-data/landuse/NLCD/nlcd2016landcover.vrt,output_file=/nfs/fwe-data/landuse/NLCD/NLCD_2016_TNC.csv countpixels.sh
