#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesyncshared
#SBATCH --job-name=cdl_byyear

# Job script to get USDA cropland tallies by region for all years that we have CDL
outdir="/nfs/fwe-data/landuse/output_csvs"

# Get year beginning with 2009 from task ID
((year=SLURM_ARRAY_TASK_ID+2008))

# Define file names
cdlraster="/nfs/fwe-data/landuse/USDAcropland/CDL/cdl${year}.vrt"
bcrvector="/nfs/fwe-data/landuse/ecoregions/bcr_usa_combined.shp"
fafvector="/nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp"
tncvector="/nfs/fwe-data/landuse/ecoregions/tnc_usa_aea.shp"

# Do zonal stats on each of the 3 shapefiles
python3 /research-home/qread/fwe/NLCD/tabulateraster.py ${bcrvector} ${cdlraster} ${outdir}/CDL_${year}_BCR.csv
python3 /research-home/qread/fwe/NLCD/tabulateraster.py ${fafvector} ${cdlraster} ${outdir}/CDL_${year}_FAF.csv
python3 /research-home/qread/fwe/NLCD/tabulateraster.py ${tncvector} ${cdlraster} ${outdir}/CDL_${year}_TNC.csv
