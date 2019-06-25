#!/bin/bash
#SBATCH -n 8
#SBATCH -t 8:00:00

Rscript --vanilla /research-home/qread/fwe/USEEIO/grid_sensitivity_parallel.r
