#!/bin/bash
#BATCH --job-name=bat_deg                              # Name of the job
#SBATCH --output=logs2/batch_deg_%j_%A.out          # STDOUT file
#SBATCH --error=logs2/batch_deg_%j_%A.err           # STDERR file
#SBATCH --nodes=1                                       # Node count
#SBATCH --ntasks=1                                      # Number of tasks across all nodes
#SBATCH --cpus-per-task=1                               # Cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=10G                                       # total memory per node
#SBATCH --array=1-10                                                # Number of jobs % Max number of jobs to consume
#SBATCH --time=95:59:59                               # Run time limit (HH:MM:SS)
#SBATCH --mail-type=all                                 # Email on job start, end, and fault
#SBATCH --mail-user=dmr4@princeton.edu

FILE=$(sed "${SLURM_ARRAY_TASK_ID}q;d" QLQRPipeline/headstotal.txt )

python scripts/gml.py DeDupModel/${FILE} Head_to_Head Time
