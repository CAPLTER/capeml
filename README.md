
<!-- README.md is generated from README.Rmd. Please edit the latter. -->
capeml: tools to aid the generation of EML metadata
===================================================

This package contains tools to aid the generation of EML metadata with intent to publish a data set (data + metadata) in the Environmental Data Initiative (EDI) data repository. Functions and a workflow are included that allow for the creation of metadata at the data set level, and individual data entities (spatial vectors, raster, other entities, data tables).

Helper functions for the creation of spatialRaster entities, and to generate a keywordSet from a template file are currently supported. Additional helper functions are planned.

### Installation

Install the current version from GitHub (after installing the `devtools` package from CRAN):

``` r
devtools::install_github("CAPLTER/capeml")
```

### generate spatialRaster

Please see the [create\_spatialRaster](https://github.com/CAPLTER/capeml/blob/master/vignettes/create_spatialRaster.Rmd) vignette for more information about creating EML for raster data.

### generate keywordSet (from file)

The function `create_keywordSet` generates a EML object of type keywordSet from a csv file containing thesaurus, keyword, and type where type is an optional keyword attribute. Keyword files should be structured like the following...

| thesaurus                   | keyword          | type  |
|:----------------------------|:-----------------|:------|
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

The object (e.g., datasetKeywords) can then be included in the EML generation script.

### generate taxonomicCoverage

The function `set_taxonomicCoverage` is borrowed from [rOpenSci EML](https://github.com/ropensci/EML) to create a EML entity of type taxonomicCoverage that can be added to the coverage element of an EML document. This function and approach is a fast-and-easy way to generate a taxonomicCoverage for resolvable taxa in a data set - it is not designed to resolve or address any challenges to resolving the ITIS identification of a taxa. That is, only taxa that are cleanly matched to the ITIS database are included in the taxonomicCoverage. As such, some taxa may not be included in the taxonomicCoverage element. For a more iterative approach that allows for resolving the corresponding ITIS ID of all taxa (but also may require taxonomic expertise), users should consider the EDI's [taxonomyCleanr](https://github.com/EDIorg/taxonomyCleanr) package.

Taxa should first be passed through the `indentify_resolvable_taxa` function to identify those taxa for which an ITIS ID is resolvable.

``` r
all_taxa <- identify_resolvable_taxa(listOfTaxa)
```

Run setTaxonomicCoverage on the subset of taxa for which an ITIS ID is resolvable.

``` r
resolved_taxa <- c(all_taxa[!is.na(all_taxa$resolve),]$taxon)a
taxaCoverage <- set_taxonomicCoverage(resolved_taxa, expand = T, db = 'itis')
```

``` r
coverage@taxonomicCoverage <- c(taxaCoverage)
```
