#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesyncshared
#SBATCH --job-name=countpxls

# CFS shapefile: tabulate NLCD land uses
# Use the FAF one since for some reason it works much quicker despite having a lot more regions
python3 /research-home/qread/fwe/NLCD/tabulateraster.py /nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp /nfs/fwe-data/landuse/NLCD/nlcd2011landcover.vrt /nfs/fwe-data/landuse/NLCD/NLCD_2011_FAF.csv

