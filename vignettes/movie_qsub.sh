#!/bin/bash
cd ~/hydflood3
qsub vignettes/movie_qsub_dessau.run -t 1-365
qsub vignettes/movie_qsub_lenzen.run -t 1-365
exit 0