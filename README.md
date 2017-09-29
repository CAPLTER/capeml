
<!-- README.md is generated from README.Rmd. Please edit the latter. -->
capeml: generate EML metadata
=============================

This package is intented to facilitate the creation of EML metadata with intent to publish data and metadata in the Environmental Data Initiative (EDI) data repository. Functions and a workflow are included that allow for the creation of metadata at the data set level, and individual data entities (spatial vectors, raster, other entities, data tables). Currently only the creation of spatialRaster entities is supported but other modules will be rolled out soon.

Installation
------------

You can install the current version from GitHub (after installing the `devtools` package from CRAN):

``` r
devtools::install_github("CAPLTER/capeml")
```

raster data
-----------

Please see the [create\_spatialRaster](https://github.com/CAPLTER/capeml/blob/master/vignettes/create_spatialRaster.Rmd) vignette for more information about creating EML for raster data.
