
<!-- README.md is generated from README.Rmd. Please edit the latter. -->

## capeml: tools to aid the generation of EML metadata

#### overview

This package contains tools to aid the generation of EML metadata with
intent to publish a data set (data + metadata) in the Environmental Data
Initiative (EDI) data repository. Functions and a workflow are included
that allow for the creation of metadata at the data set level, and
individual data entities (e.g., spatial vectors, raster, other entities,
data tables).

Helper functions for the creation of data set metadata and a dataTable
entity using the ropensci/eml are currently supported.

Helper functions for the creation of metadata for spatialRaster)
currently are built on an earlier version of ropensci/eml and not
functional (these will be updated soon (2018-12-17)), but generating
spatialVectors (output to kml) is supported.

Note that the creation of people-related entities in EML are specific to
functions that rely on Global Institute of Sustainability
infrastructure; please see the
[gioseml](https://github.com/CAPLTER/gioseml) package for these tools.

**navigation**

  - [installation](https://github.com/CAPLTER/capeml#installation)
  - [dataTable](https://github.com/CAPLTER/capeml#dataTable)
  - [otherEntity](https://github.com/CAPLTER/capeml#otherEntity)
  - [dataSet elements: keywords, taxonomic
    coverage](https://github.com/CAPLTER/capeml#dataSet)
      - [generate keyword
        set](https://github.com/CAPLTER/capeml#generate-keywordset-from-file)
      - [generate taxonomic
        coverage](https://github.com/CAPLTER/capeml#generate-taxonomiccoverage)
  - [spatialVector](https://github.com/CAPLTER/capeml#spatialVector)

currently unsupported

  - [spatial
    raster](https://github.com/CAPLTER/capeml#generate-spatialraster)

#### installation

Install the current version from GitHub (after installing the `devtools`
package from CRAN):

``` r
devtools::install_github("CAPLTER/capeml")
```

#### dataTable

Given a rectangular data matrix of type dataframe or Tibble in the R
envionment:

`write_attributes(data_entity)` will generate a template as a csv file
in the working directory based on properties of the data entity such
that metadata properties (e.g., attributeDefinition, units) can be added
via a editor or spreadsheet application.

`write_factors(data_entity)` will generate a template as a csv file in
the working directory based on columns of the data entity that are
factors such that details of factor levels can be added via a editor or
spreadsheet application.

`create_dataTable(data_entity)` performs many services:

  - the data entity is written to file as a csv in the working directory
    with the file name:
    *projectid\_data-entity-name\_md5-hash-of-file.csv*
  - metadata provided in the attributes and factors (if relevant)
    templates are ingested
  - a EML object of type dataTable is returned
  - note that the data entity name should be used consistently within
    the chunk, and the resulting dataTable entity should have the name:
    *data\_entity\_DT*

The `create_dataTable` function accepts five arguments:

1.  dfname = dataframe or tibble object name (required)
2.  description = a description of the data object (required)
3.  dateRangeField = (optional) The quoted name of the data entity field
    that is a date field that would reflect the start and end dates of
    the data reflected in the data entity. The date must be of the
    format %Y-%m-%d.
4.  baseURL = (optional) The base path of the web-accessible location of
    the data file; the name of the resulting file will be passed to the
    base path to generate a web-resolvable file path.
5.  missingValueCode = (optional) create\_dataTable will automatically
    document the presence of NA and NaN entries as missing values in the
    EML output. The user has the ability to identify an additional
    indicator of missing values. Numbers (e.g., -9999 should be
    unquoted) whereas text values (e.g., “missing”) should be quoted.

<!-- end list -->

``` r
data_entity <- read("data source") %>% 
  processing %>% 
  processing

write_attributes(data_entity)
write_factors(data_entity)

data_entity_desc <- "snow leopard data"

# create_dataTable with minimal arguments
data_entity_DT <- create_dataTable(dfname = data_entity,
                                   description = data_entity_desc)

# create_dataTable with optional arguments (sans baseURL)
data_entity_DT <- create_dataTable(dfname = data_entity,
                                   description = data_entity_desc,
                                   dateRangeField = "observation date",
                                   missingValueCode = "missing")
```

#### otherEntity

A EML object of type otherEntity can be created from a single file or a
directory. As with all objects created with the capeml package, the
resulting object is named with convention:
projectid\_object-name\_md5sum-hash.file extension. In the case of
generating a otherEntity object from a directory, a zipped file of the
directory is created.

``` r
# general use
other_entity_name <- create_otherEntity(
  targetFile = "file or directory name",
  description = "description of entity"
)

# example, create otherEntity object from a kml file
desert_fertilization_sites <- create_otherEntity(
  targetFile = "~/Desktop/desert_fertilization_sampling_sites.kml",
  description = "approximate location of desert fertiliztion long-term study sites")

# example, create otherEntity object from a pdf file
pass_codebook_2011 <- create_otherEntity(
  targetFile = "PASS-2011-Codebook-Feb2016rev.pdf",
  description = "PASS 2011 survey codebook")

# example, create otherEntity object from a directory
pass_codebook_2011 <- create_otherEntity(
  targetFile = "~/Desktop/max_temperature",
  description = "rasters of max temperature years 2000-2016")
```

#### dataSet elements: keywords, taxonomic coverage

##### keywords

`write_keywords()` will generate a template as a csv file titled
`keywords.csv` in the working directory with default CAP LTER keywords,
and the appropriate thesaurii for adding and referencing additional
keywords.

Once constructed, keywords can be added to the dataSet by referencing
the keyword file with ropensci/EML::create\_keywordSet

``` r
keywords <- create_keywordSet('keywords.csv')
```

##### taxonomic coverage

New approach for taxonomy is to use EDI’s taxonomyCleanr to build the
taxonomicCoverage. Note that at the time of this writing (2019-05-03),
taxonomyCleanr had not been ported to rOpenSci EML v2. As such, this
workflow employs a forked, feature branch of taxonomyCleanr
(<https://github.com/CAPLTER/taxonomyCleanr/tree/taxonomy-rEML2>) until
EDI adapts taxonmyCleanr to ropensci EML v2.

*Note* that the `taxa_map.csv` built with the `create_taxa_map()`
function and resolving taxonomic IDs (i.e., `resolve_comm_taxa()`) only
needs to be run once per version/session – the taxonomicCoverage can be
built as many times as needed with `resolve_comm_taxa()` or
`resolve_sci_taxa` once the `taxa_map.csv` has been generated and the
taxonomic IDs resolved.

A sample workflow:

``` r
# load and call the feature branch of taxonomyCleanr
devtools::load_all('path-to-repo/taxonomyCleanr/') 
library(taxonomyCleanr)

# taxonomyCleanr requires a path to build the taxa_map
my_path <- getwd() 

# taxonomic data must be in a data frame (load if not already in the environment)
myTaxa <- read_csv('file with taxa') %>% 
  as.data.frame()

# create or update map. A taxa_map.csv is the heart of taxonomyCleanr. This
# function will build the taxa_map.csv and put it in the path identified with
# my_path.
create_taxa_map(path = my_path,
                x = myTaxa,
                col = "myTaxa column with taxonomic names") 

# resolve taxa by attempting to match the taxon names. In this case, data.source
# 3 is ITIS (which, as of 2019-05-03, is the only authority taxonomyCleanr will
# allow for common names).

# resolve from common name:
resolve_comm_taxa(path = my_path, data.sources = 3) # in this case, 3 is ITIS

-- OR --

# resolve from scientific name:
resolve_sci_taxa(path = my_path, data.sources = 3) # in this case, 3 is ITIS

# build the EML taxonomomic coverage
taxaCoverage <- make_taxonomicCoverage(path = my_path)

# add taxonomic to other coverages
coverage$taxonomicCoverage <- taxaCoverage
```

#### spatialVector

Given a spatial vector entity in the R envionment:

`write_attributes(vector_data_entity)` will generate a template as a csv
file in the working directory based on properties of the
vector\_data\_entity such that metadata properties (e.g.,
attributeDefinition) can be added via a editor or spreadsheet
application.

`create_spatialVector(data_entity)` performs many services:

  - the spatial vector data entity is written to file as a kml in the
    working/project directory with the file name:
    *projectid\_data-entity-name.kml* (note all output is as kml)
  - metadata provided in the attributes templates are ingested
  - a EML object of type spatialVector is returned
  - note that the spatial vector data entity name should be used
    consistently within the chunk, and the resulting
    vector\_data\_entity entity should have the name: *data\_entity\_SV*

The `create_spatialVector` function accepts three arguments:

1.  svname = spatial data entity name (required)
2.  description = a description of the data object (required)
3.  baseURL = (optional) The base path of the web-accessible location of
    the data file; the name of the resulting file will be passed to the
    base path to generate a web-resolvable file path.

#### currently unsupported

##### generate spatialRaster

Please see the
[create\_spatialRaster](https://github.com/CAPLTER/capeml/blob/master/vignettes/create_spatialRaster.Rmd)
vignette for more information about creating EML for raster data.
