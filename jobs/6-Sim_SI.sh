#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=31
#SBATCH --nodes=1

#SBATCH --mail-user=theo_s@berkeley.edu
#SBATCH --mail-type=ALL

# Run Sim 6

R CMD BATCH --no-save code/Appendix/6-Sim_SI.R --n 300000 --r 30 6-Sim_SI.out
