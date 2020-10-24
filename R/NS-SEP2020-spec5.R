#!/bin/bash
#SBATCH --mail-user=$jz981@nyu.edu
#SBATCH --mail-type=END
#SBATCH --job-name=Spec5
#SBATCH --output=nsS5.out
#SBATCH --error=nsS5.err
#SBATCH --time=60:30:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16GB

set -e
module purge
module load r/intel/3.3.2
##module load jdk/1.8.0.111
##module load gcc/6.3.0
RUNDIR=$HOME/R

cd $RUNDIR

Rscript NS-cluster-r2-spec5-two-models.R
exit 0;