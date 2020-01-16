EXEC="countpixels.sh"
outdir="/nfs/qread-data/raw_data/landuse/output_csvs"
cd ~/fwe/jobscripts

# Extract NLCD2016 by combined BCR, FAF, and TNC
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=/nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt,output_file=${outdir}/NLCD_2016_BCR.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=/nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt,output_file=${outdir}/NLCD_2016_FAF.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=/nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt,output_file=${outdir}NLCD_2016_TNC.csv ${EXEC}

# Use the historic 1700 raster.
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=/nfs/qread-data/raw_data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=${outdir}/BCR1700.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=/nfs/qread-data/raw_data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=${outdir}/FAF1700.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=/nfs/qread-data/raw_data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=${outdir}/TNC1700.csv ${EXEC}

# Extract USDA Cropland Data Layer 2018 for BCR, FAF, and TNC
cdlraster="/nfs/qread-data/raw_data/landuse/USDAcropland/CDL/cdl2018.vrt"
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_BCR.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_FAF.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_TNC.csv ${EXEC}

# Extract the rasterized TNC by CFS and by US states
tncraster="/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea_gridded.tif"
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,\
	raster_file=${tncraster},\
	output_file=${outdir}/TNCcount_by_CFS.csv ${EXEC}

sbatch -J countbystate --export=vector_file=/nfs/qread-data/raw_data/landuse/USA/USA_adm1_aea.shp,\
	raster_file=${tncraster},\
	output_file=${outdir}/TNCcount_by_state.csv ${EXEC}

# Extract the rasterized BCR by CFS and by US states
bcrraster="/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_aea_gridded.tif"
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,\
	raster_file=${bcrraster},\
	output_file=${outdir}/BCRcount_by_CFS.csv ${EXEC}

sbatch -J countbystate --export=vector_file=/nfs/qread-data/raw_data/landuse/USA/USA_adm1_aea.shp,\
	raster_file=${bcrraster},\
	output_file=${outdir}/BCRcount_by_state.csv ${EXEC}

# Extract cropland data layer for all years (2018 already done)
sbatch --array=1-9 countcdlbyyear.sh
