<!-- readme.md is generated from readme.rmd. please edit the latter. -->

## capeml: tools to aid the generation of EML metadata

### overview

This package contains tools to aid the generation of EML metadata with intent
to publish a dataset (data + metadata) in the Environmental Data Initiative
(EDI) data repository. Functions and a template work flow are included that
allow for the creation of metadata at the dataset level, and individual data
entities (e.g., other entities, data tables).

Helper functions for the creation of dataset metadata for dataTable and
otherEntity objects using the [EML](https://docs.ropensci.org/EML/) package are
supported. This package can be extended with the
[capemlGIS](https://github.com/caplter/capemlgis) package to generate metadata
for spatialRaster and spatialvector objects.

A template work flow is available as part of this package. The template is
automatically generated if a new project is created with `write_directory`,
which also generates a `config.yaml` file and new directory, or with the
`write_template` function.

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
both physical objects and EML references to those objects. By default, the
package will name output files with the format
`identifier`\_`object-name`\.`file-extension` (e.g., *664_site_map.png*). The
target object (e.g., site_map.png from the previous example) is renamed with
the additional metadata and this object name is referenced in the EML metadata.
Project naming can be disabled by setting the `projectNaming` flag to `FALSE`.
When set to FALSE, the object name is not changed, and the name of the data
object as read into the R environment is written to file and referenced in the
EML. Note that the package identifier (number) is not passed as an argument,
and must exist in `config.yaml` (as `identifier`). 

### getting started

#### new projects

For new projects, `write_directory` will create a project directory at the
current (default) or specified path. The package scope and number (e.g., "edi",
521) are passed as arguments, with the package name (i.e., scope + identifier)
becoming the directory name. Within the newly created directory, a template
work flow as an Rmarkdown (Rmd) file with the package scope and number as the
file name is generated and a `config.yaml`. In `config.yaml`, the scope and
package identifier are generated as parameters. 

Creating a new project from the command line (*sensu* below) then opening it
with R is a convenient approach.

*create project from command line*


```r
R --vanilla -e 'capeml::write_directory(scope = "edi", identifier = 521)'
```

#### existing projects

For existing projects, if `config.yaml` does not already exist, `write_config`
will generate `config.yaml` with the package scope and identifier (e.g., "edi",
521) passed as an argument to the function. A version number (default = 1) can
be passed as a separate argument.

A template work flow as an Rmarkdown (Rmd) file named with the package scope
and identifier can be generated independently with the `write_template`
function.

### tools to generate entity metadata

* `write_attributes()` creates a template as a yaml file for supplying
  attribute metadata for a tabular data object that resides in the R
  environment
* `write_factors()` creates a template as a yaml file for supplying code
  definition metadata for factors in a tabular data object that resides in the
  R environment

#### tools to create EML entities

* `create_dataTable()` creates a EML entity of type dataTable
* `create_otherEntity()` creates a EML entity of type otherEntity

### construct a dataset

#### project details: dataset package number and package identifier

Package details, including scope and identifier are read from config.yaml. The
appropriate version is determined by identifying the highest version currently
in the production environment of the EDI repository (1 for new packagees).

#### title

The dataset title is read from the `title` parameter of `config.yaml`. The
title can be quoted or unquoted but must be quoted if the title contains a
colon.

#### maintenance

The maintenance status of a project is read from the `maintenance` parameter of
`config.yaml`. Standardized language is provided for either `none` (updates not
anticipated) or `regular` (approximately annual updates are anticipated)
maintenance regimes. `NULL` or text other than `none` or `regular` will omit
the `maintenance` element from the resulting EML.

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
# methods from file tagged as markdown
main <- list(description = read_markdown("methods.md"))

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
my_path <- getwd() # taxonomyCleanr requires a path (to build the taxa_map)

# Example: draw taxonomic information from existing resource:

# plant taxa listed in the om_transpiration_factors file

plantTaxa <- readr::read_csv('om_transpiration_factors.csv') |> 
  dplyr::filter(attributeName == "species") |> 
  as.data.frame()

# create or update map. A taxa_map.csv is the heart of taxonomyCleanr. This
# function will build the taxa_map.csv and put it in the path identified with
# my_path.

taxonomyCleanr::create_taxa_map(
  path = my_path,
  x    = plantTaxa,
  col  = "definition"
) 

# Example: construct taxonomic resource:

gambelQuail <- tibble::tibble(taxName = "Callipepla gambelii")

# Create or update map: a taxa_map.csv is the heart of taxonomyCleanr. This
# function will build the taxa_map.csv in the path identified with my_path.

taxonomyCleanr::create_taxa_map(
  path = my_path,
  x    = gambelQuail,
  col  = "taxName"
) 

# Resolve taxa by attempting to match the taxon name (data.source 3 is ITIS but
# other sources are accessible). Use `resolve_comm_taxa` instead of
# `resolve_sci_taxa` if taxa names are common names but note that ITIS
# (data.source 3) is the only authority taxonomyCleanr will allow for common
# names.

taxonomyCleanr::resolve_sci_taxa(
  path         = my_path,
  data.sources = 3 # ITIS
) 

# build the EML taxonomomic coverage

taxaCoverage <- taxonomyCleanr::make_taxonomicCoverage(path = my_path)

# add taxonomic to the other coverages

coverage$taxonomicCoverage <- taxaCoverage
```

#### people

For CAP LTER datasets, we can use the `gioseml` package to harvest person
details from the Global Institute of Sustainability and Innovation database.
For dataset parties not associated with the CAP LTER (or parties not in the
GIOSI database), we can construct `creator`, `metadataProvider`, and
`associatedParty` elements using the list structure provided by ROpenSci's
[EML](https://docs.ropensci.org/eml/).


```r
# creator(s) - required

# if the person has an ORCiD, generate that first

sean_orcid <- EML::eml$userId(directory = "https://orcid.org")
sean_orcid$userId <- "1111-1111-1111-1111"

sean <- EML::eml$creator(
  individualName = EML::eml$individualName(
    givenName = "Sean",
    surName   = "Payton"
    ),
  electronicMailAddress = "spayton@saints.com",
  organizationName      = "Saints",
  userId = sean_orcid
)

# sans ORCiD

kliff <- EML::eml$creator(
  individualName = EML::eml$individualName(
    givenName = "Kliff",
    surName   = "Kingsbury"
    ),
  electronicMailAddress = "kkingsbury@cardinals.com",
  organizationName      = "Cardinals"
)

creators <- list(
  sean,
  kliff
)

# metadata provider - required

pete_orcid <- EML::eml$userId(directory = "https://orcid.org")
pete_orcid$userId <- "3333-3333-3333-3333"

pete <- EML::eml$metadataProvider(
  individualName = EML::eml$individualName(
    givenName = "Pete",
    surName   = "Carrol"
    ),
  electronicMailAddress = "pcarroll@seahawks.com",
  organizationName      = "Seahawks",
  userId = pete_orcid
)

metadataProvider <- list(pete)

# associated party (optional)

# requires the additional argument `role`

brandon <- EML::eml$associatedParty(
  individualName = EML::eml$individualName(
    givenName = "Brandon",
    surName   = "Staley"
    ),
  electronicMailAddress = "bstaley@chargers.com",
  organizationName      = "Chargers",
  role                  = "head coach"
)

associatedParty <- list(brandon)
```

#### data objects

*data tables*

The `create_dataset` function will look for objects in the R environment with a
trailing `_DT` in the object name (e.g. `my_table_DT`), e.g., see section on
creating a dataTable below. All objects with that naming convention will be
added to the dataset. 

### overview: create a dataTable

Given a rectangular data matrix of type dataframe or tibble in the R
environment:

`write_attributes(data_entity)` will generate a template as a yaml file in the
working directory based on properties of the data entity such that metadata
properties (e.g., attributeDefinition, units) can be added via a editor.

`write_factors(data_entity)` will generate a template as a yaml file in the
working directory based on columns of the data entity that are factors such
that details of factor levels can be added via a editor.

Note that `write_attributes` and `write_factors` are wrapped in a `try` block.
This allows us to run the chunk or knit the entire document even if the
attributes and factors yaml files already exist (since they will not be
overwritten unless the overwrite flag is set, thus aborting the chunk).

`create_dataTable(data_entity)` performs many services:

* the data entity is written to file as a csv in the working directory with the
  file name: *identifier_data-entity-name.csv*
* metadata provided in the attributes and factors (if relevant) templates are
  ingested
* a EML object of type dataTable is returned note that the data entity name
  should be used consistently within the chunk, and the resulting dataTable
  entity should have the name: *data_entity_DT*

*create_dataTable example: my_table*


```r
my_table <- import / generate...process...

try({
  capeml::write_attributes(my_table)
  capeml::write_factors(my_table)
})

my_table_desc <- "description of table"

# create_dataTable() accepts additionalInfo but is not required

my_additional_info <- "more metadata""

my_table_DT <- capeml::create_dataTable(
  dfname                  = my_table,
  description             = my_table_desc,
  dateRangeField          = "my_date_field",
  additional_information  = my_additional_info 
)
```

### overview: create a otherEntity

A EML object of type otherEntity can be created from a single file or a
directory. In the case of generating a otherEntity object from a directory,
pass the directory path to the target_file_or_directory argument, capeml will
recognize the target as a directory, and create a zipped file of the identified
directory.

If the otherEntity object already is a zip file with the desired name, set the
overwrite argument to FALSE to prevent overwriting the existing object.

As with all objects created with the capeml package, the resulting object is
named with convention: projectid_object-name.file extension by default but this
functionality can be turned off by setting projectNaming to FALSE.

As with `create_dataTable()`, `create_otherEntity()` can also take advantage of
the `write_attributes()` and `write_factors()` services of capeml. An example
of where you might want to use these features would be when documenting a
spatial resource that cannot be coerced into a type `spatialRaster` or
`spatialVector` (e.g., because of an undocumented coordinate system). To use
these services with a directory, create an object in R with the same name as
the directory that will be zipped, then pass that object to
`write_attributes()` and `write_factors()` - capeml will look for the resulting
attribute and factor (if relevant) yaml files and match them to the directory
name (see following for an example).

*example using create_otherEntity to construct an object of type otherEntity
from an ESRI shapefile titled UEI_Features_CAPLTER_2010_2017_JAB.shp (plus
*.dbf, *.prj, and other shapefile files) that is in a directory of the same
name (sans file extension)*


```r
# Read the data into R, here a shapefile using the sf package being careful to
# name the resulting object in the R environment with the same name of the
# directory housing the shapefiles (i.e., UEI_Features_CAPLTER_2010_2017_JAB).

UEI_Features_CAPLTER_2010_2017_JAB <- sf::st_read(
  dsn   = "/path/UEI_Features_CAPLTER_2010_2017_JAB/",
  layer = "UEI_Features_CAPLTER_2010_2017_JAB"
)

# add factors if and as appropriate

UEI_Features_CAPLTER_2010_2017_JAB <- UEI_Features_CAPLTER_2010_2017_JAB |>
  dplyr::mutate(myfactor = as.factor(UEI_type))

# Generate yaml files of both the attributes and factors (if relevant) from the
# shapefile that we read into R; these will be written to the project directory
# with the name of the object that we crated in the R environment in the first
# step - again, this must correspond to the name of directory housing the files
# to be zipped.

capeml::write_attributes(UEI_Features_CAPLTER_2010_2017_JAB, overwrite = TRUE)
capeml::write_factors(UEI_Features_CAPLTER_2010_2017_JAB, overwrite = TRUE)

# an object description is required

UEI_Features_CAPLTER_2010_2017_JAB_desc <- "compilation of pre-existing data as
well as data created using expert opinion featuring (1) land-use and land-cover
data used to classify the amount of agricultural land (classified as inactive
  cropland and active cropland) as well as standing water, (2) water in canals
and along the Salt River, (3) vacant lands which were classified as bare soil,
grass/trees, or scrub, and (4) community parks and desert parks"

# create_otherEntity() accepts additionalInfo but is not required

UEI_Features_CAPLTER_2010_2017_JAB_additional <- "this is a spatial data object
with a Coordinate Reference System (CRS) of EPSG:2868 - NAD83(HARN) / Arizona
Central (ft) - Projected"

# Create the otherEntity object (i.e., UEI_other_entity) - the name of this
# object is irrelevant but should have the `_OE` suffix to identify it as an
# object of type otherEntity as a convenience when constructing the dataset. If
# the names were matched appropriately, UEI_other_entity_OE should feature all
# of the attribute metadata provided in the corresponding attribute and factor
# (if relevant) yaml files. Additionally, the target directory housing our
# shapefiles will be zipped and, possibly, renamed depending on whether project
# naming was invoked.

uei_features_OE <- capeml::create_otherEntity(
  target_file_or_directory = "data/UEI_Features_CAPLTER_2010_2017_JAB",
  description              = UEI_Features_CAPLTER_2010_2017_JAB_desc,
  additional_information   = UEI_Features_CAPLTER_2010_2017_JAB_additional
)
```


### citations

Below are sample work flows that use `capeml`'s `create_citation` function to
generate citations by passing a resource DOI to crossref. Citations can be
added to EML `literatureCited` and `usageCitation` elements. The work flow
capitalizes on EML version 2.2 that accepts the BibTex format for references.

`create_dataset()` will look for citation entities at the time of dataset
construction so desired citation entities must exist in the R environment.
`literatureCited` entities must be in a list named `citations`, and
`usageCitation` entities must be a list named `usages`.

Note that, unlike a `literatureCited` citation, a `usageCitation` is **not**
wrapped in a citation tag.


#### literature cited


```r
cook    <- capeml::create_citation("https://doi.org/10.1016/j.envpol.2018.04.013")
sartory <- capeml::create_citation("https://doi.org/10.1007/BF00031869")

citations <- list(
  citation = list(
    cook,
    sartory
  ) # close list of citations
) # close citation
```

#### usage citations


```r
brown <- capeml::create_citation("https://doi.org/10.3389/fevo.2020.569730")

usages <- list(
    brown
) # close usages

dataset$usageCitation <- usages
```

#### citations that do no have a DOI

Though a DOI makes documenting references easy, we can add citations that do
not have a DOI. There are many ways to address this but likely easiest is to
get or create a citation for the reference in bibtex format.
[bibutils](http://sourceforge.net/p/bibutils/home/Bibutils/) is a helfpul
utility that can convert other citation formats, such as .ris, to bibtex. With
bibutils, we can convert ris to an intermediate xml format and then to bibtex.


```sh

wget -O ~/Desktop/tellman_dissertation.ris https://repository.asu.edu/items/53734.ris
cat tellman_dissertation.ris | ris2xml | xml2bib >> tellman_dissertation.bib

```

Once we have the citation in bibtex format, we can add it along with other
citations as in the example below where we added the citation for the Tellman
dissertation to a suite of citations generated with capeml's create_citiation
function.


```r
tellman_2021 <- capeml::create_citation("https://doi.org/10.1016/j.worlddev.2020.105374")
lerner_2018  <- capeml::create_citation("https://doi.org/10.1016/j.cities.2018.06.009")
eakin_2019   <- capeml::create_citation("https://doi.org/10.5751/ES-11030-240315")

tellman_dissertation <- "
@phdthesis{Tellman_2019,
author={Tellman, Elizabeth
and Turner II, Billie L.
and Eakin, Hallie
and Janssen, Marco
and de Alba, Felipe
and Jain, Meha},
title={Mapping and Modeling Illicit and Clandestine Drivers of Land Use Change: Urban Expansion in Mexico City and Deforestation in Central America},
publisher={Arizona State University},
keywords={Geography; Urban planning; Land use planning; Central America; Clientelism; Institutions; Mexico; Narcotrafficking; Urbanization},
note={Doctoral Dissertation Geography 2019},
url={http://hdl.handle.net/2286/R.I.53734}
}"


bib_citation <- function() {

  eml_citation        <- EML::eml$citation(id = "http://hdl.handle.net/2286/R.I.53734")
  eml_citation$bibtex <- tellman_dissertation

  return(eml_citation)

}

tellman_2019 <- bib_citation()

usages <- list(
  tellman_2021,
  goldblatt_2018,
  lerner_2018,
  eakin_2019,
  tellman_2019 
) # close list of usages
```
