#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesyncshared
#SBATCH --job-name=countpxls

# CFS shapefile: tabulate NLCD land uses
# Use the FAF one since for some reason it works much quicker despite having a lot more regions
python3 /research-home/qread/fwe/NLCD/tabulateraster.py ${vector_file} ${raster_file} ${output_file} 
