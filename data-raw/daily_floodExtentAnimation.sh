#!/bin/bash
source /etc/profile.d/modules.sh
module purge
module load i4/applications/R-OS
module list
cd /home/WeberA/hydflood3
Rscript data-raw/daily_floodExtentAnimation.R > data-raw/daily_floodExtentAnimation.log 2>&1
exit 0
