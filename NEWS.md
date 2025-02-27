# hydflood 0.5.10

* require R >= 4.1.0
* capture download timeout errors
* add a reference to the 2023 flood3 products published on pangaea.de

# hydflood 0.5.9

* bug fixing in `getDEM()`

# hydflood 0.5.8

* replace scripted html-postprocessing with packaged (`bfgdown::cleanAll`)
* deactivate time-consuming `classifyToPNV()` example
* improve website accessibility
* move file downloads from `utils::download.file` to `httr2`
* add `waterDepth()`-function
* fix broken urls

# hydflood 0.5.7

* adapt BfG's new corporate design
* link the changelog in the pkgdown navbar

# hydflood 0.5.6

* set download.file mode = "wb" on windows
* check package on r-hub.io
* reduce spatial extent of the example of `classifyToPNV`, thereby computational efforts, to reduce check duration
* use the stable user interface of class `SpatExtent` in terra (Issue #16)
* add reference to the flood3 data computed for 2022 and published on pangaea.de

# hydflood 0.5.5

* change to documentation to avoid a problem on CRAN (Issue #13)
* final citation of the associated manuscript article

# hydflood 0.5.4

* Get rid of the last 'sp' remains
* Fix bug in `getDEM()`
* Capture `timeout` warning and recommend increased `options("timeout")`
* Improve cleanliness of `download.file` errors

# hydflood 0.5.3

* Added a `NEWS.md` file to track changes to the package.
* Changes due to package availability on CRAN.
* Improve Figure 1 of the accompanying manuscript.
* Adapt the authors formating in `inst/CITATION` to present requirements.
* Remove `tidyverse` from `Suggests`
