#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesync
#SBATCH --job-name=fafeeio

Rscript --vanilla /research-home/qread/fwe/FAF/get_eeio_for_faf.r

