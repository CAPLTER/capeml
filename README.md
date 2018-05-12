
    ## Warning: package 'kableExtra' was built under R version 3.4.4

<!-- README.md is generated from README.Rmd. Please edit the latter. -->

# capeml: tools to aid the generation of EML metadata

This package contains tools to aid the generation of EML metadata with
intent to publish a dataset (data + metadata) in the Environmental Data
Initiative (EDI) data repository. Functions and a workflow are included
that allow for the creation of metadata at the data set level, and
individual data entities (spatial vectors, raster, other entities, data
tables).

Helper functions for the creation of spatialRaster entities, and to
generate a keywordSet from a template file are currently supported.
Additional helper functions are planned.

### Installation

Install the current version from GitHub (after installing the `devtools`
package from CRAN):

``` r
devtools::install_github("CAPLTER/capeml")
```

### generate spatialRaster

Please see the
[create\_spatialRaster](https://github.com/CAPLTER/capeml/blob/master/vignettes/create_spatialRaster.Rmd)
vignette for more information about creating EML for raster data.

### generate keywordSet (from file)

The function `create_keywordSet` generates a EML object of type
keywordSet from a csv file containing thesaurus, keyword, and type where
type is an optional keyword attribute. Keyword files should be
structured like the following…

| thesaurus                   | keyword          | type  |
| :-------------------------- | :--------------- | :---- |
| LTER controlled vocabulary  | nutrients        | theme |
| LTER controlled vocabulary  | nitrate          | theme |
| LTER core areas             | water and fluxes | theme |
| LTER core areas             | parks and rivers | theme |
| Creator Defined Keyword Set | stormwater       | theme |
| Creator Defined Keyword Set | catchment        | theme |
| CAPLTER Keyword Set List    | arid land        | theme |
| CAPLTER Keyword Set List    | az               | place |

Call the function:

``` r
datasetKeywords <- create_keywordSet('path/keywordFile.csv')
```

The object (e.g., datasetKeywords) can then be included in the EML
generation script.
