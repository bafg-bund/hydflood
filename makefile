SHELL:=/bin/bash

install:
	. /usr/share/Modules/init/bash; \
	module purge; \
	module load i4/R/latest; \
	module list; \
	source /opt/rh/devtoolset-8/enable; \
	Rscript _install.R

pkg:
	. /usr/share/Modules/init/bash; \
	module purge; \
	module load i4/R/latest; \
	module list; \
	Rscript _build.R

syncel2scr:
	rsync -av --include='*.tif' --exclude='*' /home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_UFD/data/tiff/flood3/ /scratch/webera/flood3/

syncrh2scr:
	rsync -av --include='*.tif' --exclude='*' /home/WeberA/freigaben/U/U3/Auengruppe_INFORM/RH_336_867_UFD/data/tiff/flood3/ /scratch/webera/flood3/

syncscr2el:
	rsync -av --include='e*.tif' --exclude='*' /scratch/webera/flood3/ /home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_UFD/data/tiff/flood3/

syncscr2rh:
	rsync -av --include='r*.tif' --exclude='*' /scratch/webera/flood3/ /home/WeberA/freigaben/U/U3/Auengruppe_INFORM/RH_336_867_UFD/data/tiff/flood3/

mvlog:
	mv /home/WeberA/sbatch/log/flood3_* /home/WeberA/hydflood/slurm/log/

