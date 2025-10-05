#PCA analysis on barley
#PLINK (v1.9)
#!/bin/bash
# Created by the University of Melbourne job script generator for SLURM
# Fri Mar 01 2024 11:40:23 GMT+1100 (Australian Eastern Daylight Time)

# Partition for the job:
#SBATCH --partition=cascade

# Multithreaded (SMP) job: must run on one node 
#SBATCH --nodes=1

# The name of the job:
#SBATCH --job-name="barley_PCA"

# The project ID which this job should run under:
#SBATCH --account="punim1869"

# Maximum number of tasks/CPU cores used by the job:
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# The amount of memory in megabytes per node:
#SBATCH --mem=15360

# The maximum running time of the job in days-hours:mins:sec
#SBATCH --time=0-3:0:00

# check that the script is launched with sbatch
if [ "x$SLURM_JOB_ID" == "x" ]; then
   echo "You need to submit your job to the queuing system with sbatch"
   exit 1
fi

# Run the job from the directory where it was launched (default)

# The modules to load:
module load PLINK/1.9b_6.21-x86_64

# The job command(s):
plink -bfile barley_data --pca --allow-extra-chr --mind 

##DO NOT ADD/EDIT BEYOND THIS LINE##
##Job monitor command to list the resource usage
my-job-stats -a -n -s
