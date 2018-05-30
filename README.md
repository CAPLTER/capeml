
<!-- README.md is generated from README.Rmd. Please edit the latter. -->
capeml: tools to aid the generation of EML metadata
===================================================

### overview

This package contains tools to aid the generation of EML metadata with intent to publish a data set (data + metadata) in the Environmental Data Initiative (EDI) data repository. Functions and a workflow are included that allow for the creation of metadata at the data set level, and individual data entities (spatial vectors, raster, other entities, data tables).

Helper functions for the creation of spatialRaster entities, and to generate a keywordSet from a template file are currently supported. Additional helper functions are planned.

**navigation**

-   [installation](https://github.com/CAPLTER/capeml#installation)
-   [spatial raster](https://github.com/CAPLTER/capeml#generate-spatialraster)
-   [generate keyword set](https://github.com/CAPLTER/capeml#generate-keywordset-from-file)
-   [generate taxonomic coverage](https://github.com/CAPLTER/capeml#generate-taxonomiccoverage)

<!-- - [generate kml](https://github.com/CAPLTER/capeml#generate-taxonomiccoverage) -->
### installation

Install the current version from GitHub (after installing the `devtools` package from CRAN):

``` r
devtools::install_github("CAPLTER/capeml")
```

### generate spatialRaster

Please see the [create\_spatialRaster](https://github.com/CAPLTER/capeml/blob/master/vignettes/create_spatialRaster.Rmd) vignette for more information about creating EML for raster data.

### generate keywordSet (from file)

The function `create_keywordSet` generates a EML object of type keywordSet from a csv file containing thesaurus, keyword, and type where type is an optional keyword attribute. Keyword files should be structured like the following...

<table>
<thead>
<tr>
<th style="text-align:left;">
thesaurus
</th>
<th style="text-align:left;">
keyword
</th>
<th style="text-align:left;">
type
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
LTER controlled vocabulary
</td>
<td style="text-align:left;">
nutrients
</td>
<td style="text-align:left;">
theme
</td>
</tr>
<tr>
<td style="text-align:left;">
LTER controlled vocabulary
</td>
<td style="text-align:left;">
nitrate
</td>
<td style="text-align:left;">
theme
</td>
</tr>
<tr>
<td style="text-align:left;">
LTER core areas
</td>
<td style="text-align:left;">
water and fluxes
</td>
<td style="text-align:left;">
theme
</td>
</tr>
<tr>
<td style="text-align:left;">
LTER core areas
</td>
<td style="text-align:left;">
parks and rivers
</td>
<td style="text-align:left;">
theme
</td>
</tr>
<tr>
<td style="text-align:left;">
Creator Defined Keyword Set
</td>
<td style="text-align:left;">
stormwater
</td>
<td style="text-align:left;">
theme
</td>
</tr>
<tr>
<td style="text-align:left;">
Creator Defined Keyword Set
</td>
<td style="text-align:left;">
catchment
</td>
<td style="text-align:left;">
theme
</td>
</tr>
<tr>
<td style="text-align:left;">
CAPLTER Keyword Set List
</td>
<td style="text-align:left;">
arid land
</td>
<td style="text-align:left;">
theme
</td>
</tr>
<tr>
<td style="text-align:left;">
CAPLTER Keyword Set List
</td>
<td style="text-align:left;">
az
</td>
<td style="text-align:left;">
place
</td>
</tr>
</tbody>
</table>
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

### generate otherEntity with dataType kml

The function `create_KML`...

``` r
desert_fertilization_sites <- create_KML(
  kmlFile = "~/Desktop/desert_fertilization_sampling_sites.kml",
  description = "approximate location of desert fertiliztion long-term study sites")
```
