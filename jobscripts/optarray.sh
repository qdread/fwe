#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesync
#SBATCH --job-name=optarray

Rscript --vanilla /research-home/qread/fwe/USEEIO/opt_sensitivity_parallel.r
