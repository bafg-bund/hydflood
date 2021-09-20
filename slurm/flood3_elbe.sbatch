#!/bin/bash
#SBATCH --job-name=flood3elbe
#SBATCH --mail-type=FAIL,BEGIN,END
#SBATCH --mail-user=arnd.weber@bafg.de
#SBATCH --partition=CentOS              # queue for array jobs without mpiexec/mpirun
#SBATCH --array=1-49%3                  # 1-49%10
#SBATCH --ntasks=1                      # number of tasks per node
#SBATCH --time=20-00:00                 # time DD-HH:MM
#SBATCH --error=log/flood3_elbe_%A_%4a.err
#SBATCH --output=log/flood3_elbe_%A_%4a.out
#SBATCH --get-user-env
#SBATCH --export=ALL

#---------------------------------------------------------------
# !!! job will be executed in directoy of job submission !!!
#---------------------------------------------------------------
export WORKDIR=/home/WeberA/sbatch
cd $WORKDIR

#---------------------------------------------------------------
# set module environment on the compute nodes
#---------------------------------------------------------------
module add i4/R/4.1.0
module list

#---------------------------------------------------------------
# set major binaries ( MPI executable, model executable, etc )
#---------------------------------------------------------------
MODELEXE=/opt/i4/R-4.1.0/bin/Rscript
MODELSCRIPT=flood3_elbe.R

#----------------------------------------------------------------
# Output Environment Variables
#----------------------------------------------------------------
echo "=========================================="
echo " ID of ArrayJobs           =" $SLURM_ARRAY_JOB_ID
echo " ID of ArrayTasks          =" $SLURM_ARRAY_TASK_ID
echo " Cluster Name              =" $SLURM_CLUSTER_NAME
echo " Cpus per Task             =" $SLURM_CPUS_PER_TASK
echo " Account Name              =" $SLURM_JOB_ACCOUNT
echo " Job ID                    =" $SLURM_JOB_ID
echo " Job Name                  =" $SLURM_JOB_NAMEhtop
echo " Job allocated Nodes       =" $SLURM_JOB_NODELIST
echo " Number of nodes requested =" $SLURM_JOB_NUM_NODES
echo " Parititon /Queue of Job   =" $SLURM_JOB_PARTITION
echo " User UID  Job Owner       =" $SLURM_JOB_UID
echo " User Job Owner            =" $SLURM_JOB_USER
echo " Job restart count         =" $SLURM_RESTART_COUNT
echo " Job Task ID               =" $SLURM_PROCID
echo " Job step ID               =" $SLURM_STEP_ID
echo " numer of MPI ranks        =" $SLURM_STEP_NUM_TASKS
#----------------------------------------------------------------
# Output of job information
#----------------------------------------------------------------
echo -e "\nJob: $SLURM_JOB_NAME   JobID: $SLURM_JOB_ID"
echo "=========================================="
echo "Number of nodes requested: "$SLURM_JOB_NUM_NODES
echo "Number of cores requested: "$SLURM_CPUS_ON_NODE
echo "PATH:            " $PATH
echo "LD_LIBRARY_PATH: " $LD_LIBRARY_PATH
echo "INCLUDE:         " $INCLUDE
echo -e "Current directory:" $PWD
echo "=========================================="
echo -e "\nModel output:"
echo $MODELEXE" "$MODELSCRIPT" "$SLURM_ARRAY_TASK_ID

$MODELEXE $MODELSCRIPT > log/flood3_elbe_${SLURM_ARRAY_JOB_ID}_`printf "%04d" $SLURM_ARRAY_TASK_ID`.log 2>&1 

#----------------------------------------------------------------
# EOF
#----------------------------------------------------------------

