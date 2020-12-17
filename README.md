---
output: github_document
---

<!-- readme.md is generated from readme.rmd. please edit the latter. -->

## capeml: tools to aid the generation of eml metadata

### overview

This package contains tools to aid the generation of eml metadata with intent
to publish a dataset (data + metadata) in the Environmental Data Initiative
(edi) data repository. Functions and a template work flow are included that
allow for the creation of metadata at the dataset level, and individual data
entities (e.g., other entities, data tables).

Helper functions for the creation of dataset metadata, `datatable()`, and
`otherentity()` entities using the [EML](https://docs.ropensci.org/eml/)
package are supported. This package can be extended with the
[capemlGIS](https://github.com/caplter/capemlgis) package to generate metadata
for `spatialraster()` and `spatialvector()` entities.

Note that the creation of people-related entities in the templated work flow
(see below) are specific to functions that rely on Global Institute of
Sustainability and Innovation infrastructure; other users should add people
using the list structure provided by ROpenSci's
[EML](https://docs.ropensci.org/eml/). 

A template work flow is available as part of this package. The template is
automatically generated if a new project is created with `write_directory`,
which also generates a `config.yaml` file and new directory, or with the
`write_template` function. For Vim users, additional resources for templating
are available through [capeml-vim](https://gitlab.com/caplter/capeml-vim).


### installation

Install from GitHub (after installing the
[devtools](https://cran.r-project.org/web/packages/devtools/index.html) package:


```r
devtools::install_github("CAPLTER/capeml")
```

### options

#### EML

This package defaults to the current version of EML. Users can switch to the
previous version with `emld::eml_version("eml-2.1.1")`.

#### project naming

Most EML-generating functions in the capeml and capemlGIS packages will create
both physical objects and EML references to those objects with the format:
`project-id`\_`object-name`\_`object-hash`\.`file-extension` (e.g.,
*664_site_map_5fb7b8d53d48010eab1a2e73db7f1941.png*). The target object (e.g.,
site_map.png from the previous example) is renamed with the additional metadata
and this object name is referenced in the EML metadata. The only exception to
this approach are spatialVectors (see capemlGIS) where the hash of the
file/object is not included in the new object name. Note that the project-id is
not passed to any of the functions, and must exist in `config.yaml` (as
`projectid`).

Project-naming functionality can be turned off by setting the `projectNaming`
option in `create_dataTable()` (*forthcoming*) and `create_spatialRaster()`
(also `create_spatialVector()` and `create_spatialRaster()` from capemlGIS) to
FALSE.  When set to FALSE, the object name is not changed, and the file name of
the object is included in the EML.

### getting started

#### new projects

For new projects, `write_directory` will create a project directory at the
current (default) or specified path. The package scope and number (e.g.,
edi.521) is passed as an argument, with the package identifier (sans the
version number) becoming the directory name. Within the newly created
directory, a template work flow as an Rmarkdown (Rmd) file with the package
scope and number as the file name is generated and a `config.yaml`. In
`config.yaml`, the package number and package identifier are generated as
parameters. New projects assume a version number = 1, from which the package
identifier (as `packageIdent`) is generated in `config.yaml`.

Creating a new project from the command line (*sensu* below) then opening it
with R is a convenient approach.

*create project from command line*


```r
R --vanilla -e 'capeml::write_directory(packageScopeNumber="edi.521")'
```

#### existing projects

For existing projects, if `config.yaml` does not already exist, `write_config`
will generate `config.yaml` with the package scope and number (e.g., edi.521)
passed as an argument to the function. A version number (default = 1) can be
passed as a separate argument.

A template work flow as an Rmarkdown (Rmd) file named with the package scope
and number can be generated independently with the `write_template` function.


### tools to generate entity metadata

* `write_attributes()` creates a template as a yaml file for supplying
  attribute metadata for a tabular data object that resides in the R
  environment
* `write_factors()` creates a template as a yaml file for supplying code
  definition metadata for factors in a tabular data object that resides in the
  R environment

### tools to create EML entities

* `create_dataTable()` creates a EML entity of type dataTable
* `create_otherEntity()` creates a EML entity of type otherEntity

### construct a dataset

#### project details: dataset package number and package identifier

The dataset number (e.g., 521) is read from the `packageNum` parameter of
`config.yaml`. The package identifier (e.g. edi.521.4) is read from the
`packageIdent` parameter of `config.yaml`.

#### title

The dataset title is read from the `title` parameter of `config.yaml`. The
title can be quoted or unquoted but must be quoted if the title contains a
colon.

#### abstract

The `create_dataset` function will look for a `abstract.md` file in the working
directory or at the path provided if specified. `abstract.md` must be a markdown
file.

#### keywords

`write_keywords()` creates a template as a csv file for supplying dataset
keywords. The `create_dataset` function will look for a `keywords.csv` file in
the working directory or at the path provided if specified.

#### methods

The `create_dataset` function will look for a `methods.md` file in the working
directory or at the path provided if specified (`methods.md` must be a markdown
file).

Alternatively, the work flow below is an enhanced approach of developing
methods if provenance data are required or there are multiple methods files. If
`enhancedMethods` exists, it will override arguments passed to `methodsFile` in
`create_dataset`.


```r
library(EDIutils)

# methods from file tagged as markdown
main <- read_markdown("methods.md")

# provenance: naip
naip <- emld::as_emld(EDIutils::api_get_provenance_metadata("knb-lter-cap.623.1"))
naip$`@context` <- NULL
naip$`@type` <- NULL

# provenance: lst
landSurfaceTemp <- emld::as_emld(EDIutils::api_get_provenance_metadata("knb-lter-cap.677.1"))
landSurfaceTemp$`@context` <- NULL
landSurfaceTemp$`@type` <- NULL

enhancedMethods <- EML::eml$methods(methodStep = list(main, naip, landSurfaceTemp))
```

#### coverages

*Geographic* and *temporal* coverages are straight foward and documented in the
work flow, but creating a *taxonomic* coverage is more involved. *Taxonomic
coverage(s)* are constructed using EDI's
[taxonomyCleanr](https://github.com/EDIorg/taxonomyCleanr) tool suite.

A sample work flow for creating a taxonomic coverage:


```r
library(taxonomyCleanr)

my_path <- getwd() # taxonomyCleanr requires a path (to build the taxa_map)

# Example: draw taxonomic information from existing resource:

# plant taxa listed in the om_transpiration_factors file
plantTaxa <- read_csv('om_transpiration_factors.csv') %>% 
  filter(attributeName == "species") %>% 
  as.data.frame()

# create or update map. A taxa_map.csv is the heart of taxonomyCleanr. This
# function will build the taxa_map.csv and put it in the path identified with
# my_path.
create_taxa_map(path = my_path, x = plantTaxa, col = "definition") 

# Example: construct taxonomic resource:

gambelQuail <- tibble(taxName = "Callipepla gambelii")

# Create or update map: a taxa_map.csv is the heart of taxonomyCleanr. This
# function will build the taxa_map.csv in the path identified with my_path.
create_taxa_map(path = my_path, x = gambelQuail, col = "taxName") 

# Resolve taxa by attempting to match the taxon name (data.source 3 is ITIS but
# other sources are accessible). Use `resolve_comm_taxa` instead of
# `resolve_sci_taxa` if taxa names are common names but note that ITIS
# (data.source 3) is the only authority taxonomyCleanr will allow for common
# names.
resolve_sci_taxa(path = my_path, data.sources = 3) # in this case, 3 is ITIS

# build the EML taxonomomic coverage
taxaCoverage <- make_taxonomicCoverage(path = my_path)

# add taxonomic to the other coverages
coverage$taxonomicCoverage <- taxaCoverage
```

### overview: create a dataTable

Given a rectangular data matrix of type dataframe or tibble in the R
environment:

`write_attributes(data_entity)` will generate a template as a yaml file in the
working directory based on properties of the data entity such that metadata
properties (e.g., attributeDefinition, units) can be added via a editor.

`write_factors(data_entity)` will generate a template as a yaml file in the
working directory based on columns of the data entity that are factors such
that details of factor levels can be added via a editor.

`create_dataTable(data_entity)` performs many services:

* the data entity is written to file as a csv in the working directory with the
  file name: *projectid_data-entity-name_md5-hash-of-file.csv*
* metadata provided in the attributes and factors (if relevant) templates are
  ingested
* a EML object of type dataTable is returned note that the data entity name
  should be used consistently within the chunk, and the resulting dataTable
  entity should have the name: *data_entity_DT*

*create_dataTable example: my_table*


```r
my_table <- import / generate...process...

write_attributes(my_table)
write_factors(my_table)

my_table_desc <- "description of table"

my_table_DT <- create_dataTable(
  dfname = my_table,
  description = my_table_desc
)
```

### overview: create a otherEntity

A EML object of type otherEntity can be created from a single file or a
directory. In the case of generating a otherEntity object from a directory,
pass the directory path to the targetFile argument, capeml will recognize the
target as a directory, and create a zipped file of the identified directory.

If the otherEntity object already is a zip file with the desired name, set the
overwrite argument to FALSE to prevent overwriting the existing object.

As with all objects created with the capeml package, the resulting object is
named with convention: projectid_object-name_md5sum-hash.file extension by
default but this functionality can be turned off by setting projectNaming to
FALSE.

### literature cited

Below is a sample work flow that uses `capeml`'s `create_citation` function to
generate citations by passing a resource DOI to crossref. Citations can be
added to EML `literatureCited`. The work flow capitalizes on EML version 2.2
that accepts the BibTex format for references.


```r
cook <- create_citation("https://doi.org/10.1016/j.envpol.2018.04.013")
sartory <- create_citation("https://doi.org/10.1007/BF00031869")

citations <- list(
  citation = list(
    cook,
    sartory
  ) # close list of citations
) # close citation

dataset$literatureCited <- citations
```
