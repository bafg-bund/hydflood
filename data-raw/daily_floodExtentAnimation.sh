#!/bin/bash
source /etc/profile.d/modules.sh
module purge
module load R/latest
module list
cd /home/WeberA/hydflood
Rscript data-raw/daily_floodExtentAnimation.R > data-raw/daily_floodExtentAnimation.log 2>&1
exit 0
