#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesync
#SBATCH --job-name=countpxls

# CFS shapefile: tabulate NLCD land uses
# Use the FAF one since for some reason it works much quicker despite having a lot more regions
python3 /research-home/qread/fwe/NLCD/tabulateraster.py /nfs/fwe-data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/Freight_Analysis_Framework_Regions.shp /nfs/fwe-data/landuse/NLCD/NLCD_2016_Land_Cover_L48_20190424.img /nfs/fwe-data/landuse/NLCD/NLCD_2016_FAF.csv

