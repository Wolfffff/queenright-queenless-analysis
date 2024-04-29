#!/bin/bash
#BATCH --job-name=bat_dir                              # Name of the job
#SBATCH --output=logs/batch_dir_%j.out          # STDOUT file
#SBATCH --error=logs/batch_dir_%j.err           # STDERR file
#SBATCH --nodes=1                                       # Node count
#SBATCH --ntasks=1                                      # Number of tasks across all nodes
#SBATCH --cpus-per-task=1                               # Cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=32G                                       # total memory per node
#SBATCH --array=1-966                                                # Number of jobs % Max number of jobs
#SBATCH --time=5:59:59                               # Run time limit (HH:MM:SS)
#SBATCH --mail-type=all                                 # Email on job start, end, and fault
#SBATCH --mail-user=dmr4@princeton.edu

module load conda

conda activate naps

FILE=$(sed "${SLURM_ARRAY_TASK_ID}q;d" QLQRPipeline/total.txt )

python scripts/dirtoundir.py QLQRPipeline/${FILE}
