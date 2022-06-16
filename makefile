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

www:
	rsync -auv --delete --exclude apps/flood3daily /srv/cifs-mounts/WeberA_home/WeberA/hydflood/docs/ /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hydflood
	find /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hydflood/. -iname "*.html" -exec sed -i -e 's#<a class="external-link dropdown-item" href="http://r.bafg.de/shiny/WeberA/07-flood3/">flood3</a># #g' {} \;
	find /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hydflood/. -iname "*.html" -exec sed -i -e 's#http://r.bafg.de/shiny/WeberA/08-flood3wms/#https://hydflood.bafg.de/apps/flood3wms/#g' {} \;
	find /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hydflood/. -iname "*.html" -exec sed -i -e 's#http://r.bafg.de/shiny/WeberA/10-flood3daily/#https://hydflood.bafg.de/apps/flood3daily/#g' {} \;

aq:
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' DESCRIPTION
	sed -i -e 's#http://gitlab.lan.bafg.de/auenoekologie#https://git.aqualogy.de/arnd#g' DESCRIPTION
	sed -i -e 's#gitlab.lan.bafg.de/auenoekologie#git.aqualogy.de/arnd#g' README.Rmd
	sed -i -e 's#gitlab.lan.bafg.de/auenoekologie#git.aqualogy.de/arnd#g' README.md
	sed -i -e 's#gitlab.lan.bafg.de/auenoekologie#git.aqualogy.de/arnd#g' vignettes/hydflood.Rmd
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' vignettes/hydflood.Rmd
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' pkgdown/_pkgdown.yml
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/07-#https://www.aqualogy.de/shiny/#g' pkgdown/_pkgdown.yml
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/08-#https://www.aqualogy.de/shiny/#g' pkgdown/_pkgdown.yml
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/10-#https://www.aqualogy.de/shiny/#g' pkgdown/_pkgdown.yml
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' inst/CITATION
