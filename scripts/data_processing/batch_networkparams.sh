#!/bin/bash
#BATCH --job-name=bat_nwp                              # Name of the job
#SBATCH --output=logs/batch_nwp_%j_%A.out          # STDOUT file
#SBATCH --error=logs/batch_nwp_%j_%A.err           # STDERR file
#SBATCH --nodes=1                                       # Node count
#SBATCH --ntasks=1                                      # Number of tasks across all nodes
#SBATCH --cpus-per-task=1                               # Cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=18G                                       # total memory per node
#SBATCH --array=1-969                                               # Number of jobs % Max number of jobs to consume
#SBATCH --time=05:59:00                               # Run time limit (HH:MM:SS)
#SBATCH --mail-type=all                                 # Email on job start, end, and fault
#SBATCH --mail-user=dmr4@princeton.edu

module load conda

conda activate naps

FILE=$(sed "${SLURM_ARRAY_TASK_ID}q;d" QLQRPipeline/total.txt  )

python scripts/network_params.py QLQRPipeline/${FILE}_Ints.txt QLQRPipeline/${FILE}_NWPBout.csv Head_to_Head True
