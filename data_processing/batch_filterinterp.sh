#!/bin/bash
#BATCH --job-name=bat_filt                              # Name of the job
#SBATCH --output=logs2/batch_filt_%j_%A.out          # STDOUT file
#SBATCH --error=logs2/batch_filt_%j_%A.err           # STDERR file
#SBATCH --nodes=1                                       # Node count
#SBATCH --ntasks=1                                      # Number of tasks across all nodes
#SBATCH --cpus-per-task=1                               # Cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=28G                                       # total memory per node
#SBATCH --array=1-961                                                # Number of jobs % Max number of jobs to consume
#SBATCH --time=1-00:00:00                               # Run time limit (HH:MM:SS)
#SBATCH --mail-type=all                                 # Email on job start, end, and fault
#SBATCH --mail-user=dmr4@princeton.edu

module load conda

source activate

conda activate naps

FILE=$(sed "${SLURM_ARRAY_TASK_ID}q;d" QLQRPipeline/total.txt    )

python scripts/filterinterp.py NewQueenUpdatedNaps/${FILE} txt/experimental_tag_list.csv 
