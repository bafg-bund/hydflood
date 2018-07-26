#!/bin/bash
cd ~/hydflood3
qsub vignettes/movie_qsub.run -t 1-365
exit 0