#!/bin/bash

#SBATCH --account=csu-general
#SBATCH --qos normal
#SBATCH --partition=shas
#SBATCH --job-name=simplin_lb
#SBATCH --output=run_lb.out
#SBATCH --error=run_lb.err
#SBATCH --ntasks=20
#SBATCH --mem=2000
#SBATCH --time 00:10:00

module purge
module load intel
module load impi
module load R
module load loadbalance

mpirun lb lb_sim

