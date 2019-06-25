#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesync

Rscript --vanilla /research-home/qread/fwe/USEEIO/grid_sensitivity_parallel.r
