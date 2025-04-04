

<!-- readme.md is generated from readme.rmd. please edit the latter. -->

<br> <br>

### capeml: tools to aid the generation of EML metadata

### overview

This package contains tools to aid the generation of EML metadata with
intent to publish a dataset (data + metadata) in the Environmental Data
Initiative (EDI) data repository. Functions and a template work flow are
included that allow for the creation of metadata at the dataset level,
and individual data entities (e.g., other entities, data tables).

Helper functions for the creation of dataset metadata for dataTable and
otherEntity objects using the [EML](https://docs.ropensci.org/EML/)
package are supported. This package can be extended with the
[capemlGIS](https://github.com/caplter/capemlgis) package to generate
metadata for spatialRaster and spatialVector objects.

A template work flow is available as part of this package. The template
is automatically generated if a new project is created with
`write_directory`, which also generates a `config.yaml` file and new
directory, or with the `write_template` function.

### installation

Install from GitHub (after installing the
[devtools](https://cran.r-project.org/web/packages/devtools/index.html)
package):

``` r
devtools::install_github("CAPLTER/capeml")
```

### options

#### EML version

This package defaults to the current version of EML. Users can switch to
the previous version with `emld::eml_version("eml-2.1.1")`.

#### project naming

Most EML-generating functions in the capeml and capemlGIS packages will
create both physical objects and EML references to those objects. By
default, the package will name output files with the format
`identifier`\_`object-name`.`file-extension` (e.g., *664_site_map.png*).
The target object (e.g., my_map.png) is renamed with the additional
metadata and this object name is referenced in the EML metadata. Project
naming can be disabled by setting the `projectNaming` flag to `FALSE`.
When set to FALSE, the object name is not changed, and the name of the
data object as read into the R environment is written to file and
referenced in the EML. Note that the package identifier (number) is not
passed as an argument, and must exist in `config.yaml` (as
`identifier`).

### getting started

#### new projects

For new projects, `write_directory` will create a project directory at
the current (default) or specified path. The package scope and number
(e.g., “edi”, 521) are passed as arguments, with the package name (i.e.,
scope + identifier) becoming the directory name. Within the newly
created directory, a template work flow as a Quarto (qmd) file with the
package scope and number as the file name is generated. Additional files
include a `config.yaml` for providing project-level metadata, a
`people.yaml` for providing project personnel details (see below), and a
`keywords.csv` file for providing project keywords. In `config.yaml`,
the provided scope and package identifier are generated as parameters.
Note that each of these template files can be generated outside of
`write_directory` with package functions (see below).

Creating a new project from the command line (*sensu* below) then
opening it with R is a convenient approach.

*create project from command line*

``` r
R --vanilla -e 'capeml::write_directory(scope = "knb-lter-cap", identifier = 716)'
```

#### existing projects

For existing projects, we can generate any of the needed configuration
files with package functions:

- `write_config` generates `config.yaml` with the package scope and
  identifier (e.g., “edi”, 521) passed as an argument to the function. A
  version number (default = 1) can be passed as a separate argument.
- `write_template` generates a template work flow as a Quarto (qmd) file
  named with the package scope and identifier.
- `write_people_template` generates a template yaml file for providing
  metadata regarding project personnel.
- `write_keywords` generates a template csv file for providing metadata
  regarding project keywords.

### construct a dataset

#### project details: dataset package number and package identifier

Package details, including scope and identifier are read from
config.yaml. The appropriate version is determined by identifying the
highest version currently in the production environment of the EDI
repository (1 for new packages).

#### title

The dataset title is read from the `title` parameter of `config.yaml`.
The title can be quoted or unquoted but must be quoted if the title
contains a colon.

#### maintenance

The maintenance status of a project is read from the `maintenance`
parameter of `config.yaml`. Standardized language is provided for either
`none` (updates not anticipated) or `regular` (approximately annual
updates are anticipated) maintenance regimes. `NULL` or text other than
`none` or `regular` will omit the `maintenance` element from the
resulting EML.

#### abstract

The `create_dataset` function will look for a `abstract.md` file in the
working directory or at the path provided if specified. `abstract.md`
must be a markdown file.

#### keywords

`write_keywords` creates a template as a csv file for supplying dataset
keywords. The `create_dataset` function will look for a `keywords.csv`
file in the working directory or at the path provided if specified.

#### methods

The `create_dataset` function will look for a `methods.md` file in the
working directory or at the path provided if specified (`methods.md`
must be a markdown file).

Alternatively, the work flow below is an approach of developing methods
if provenance data are required or there are multiple methods files.

``` r
# methods from file tagged as markdown
main <- list(description = capeml::read_markdown("methods.md"))

# provenance: naip
naip            <- emld::as_emld(EDIutils::get_provenance_metadata("knb-lter-cap.623.1"))
naip$`@context` <- NULL
naip$`@type`    <- NULL

# provenance: lst
landSurfaceTemp            <- emld::as_emld(EDIutils::get_provenance_metadata("knb-lter-cap.677.1"))
landSurfaceTemp$`@context` <- NULL
landSurfaceTemp$`@type`    <- NULL

rich_methods <- EML::eml$methods(
  methodStep = list(
    main,
    naip,
    landSurfaceTemp
  )
)
```

#### coverages

*Geographic* and *temporal* coverages are straightforward and documented
in the work flow, but creating a *taxonomic* coverage is more involved.
*Taxonomic coverage(s)* are constructed using EDI’s
[taxonomyCleanr](https://github.com/EDIorg/taxonomyCleanr) tool suite.

A sample work flow for creating a taxonomic coverage:

``` r
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

Project personnel metadata in the form of `<creator>`,
`<metadataProvider>`, and `<associatedParty>` are provided via the
`people.yaml` configuration file. The following example illustrates
personnel metadata for two `<creators>`, and one each
`<metadataProvider>` and `<associatedParty>`.

``` yaml
- last_name: Gannon
  first_name: Richard
  middle_name: ~
  role_type: creator
  email: rgannon@cardinals.usfl
  orcid: 1111-1111-11x1-1111
  data_source: ~
- last_name: Carrol
  first_name: Pete
  middle_name: ~
  role_type: creator
  email: pcarroll@seahawks.usfl
  orcid: 2222-2x22-2222-2222
  data_source: ~
- last_name: Payton
  first_name: Sean
  middle_name: ~
  role_type: metadataProvider
  email: spayton@broncos.usfl
  orcid: ~
  data_source: ~
- last_name: Jim
  first_name: Harbaugh
  middle_name: ~
  role_type: associatedParty
  project_role: "head coach"
  email: jharbaugh@chargers.usfl
  orcid: 3x33-3333-3333-2222
  data_source: ~
```

If personnel are involved with many or repeated projects, it may be
easier to keep personnel metadata in a file that `people.yaml` can
reference. Below is an example of the same personnel metadata but
drawing from a tabular csv file of personnel metadata. In this case, the
tabular csv file contains most of the details (e.g., email, orcid) so we
do not have to include those details in the yaml, and partial matching
is supported so we do not have to pass the full names. We pass the
location of the personnel tabular metadata file with `data_source`. We
can also mix and match providing metadata via yaml and drawing from a
tabular file. For example metadata pertaining to Pete Carrol are passed
via yaml whereas metadata for all other personnel are drawn from the
tabular file, with the presence of a `data_source` providing the
indication to generate EML metadata from the details provided in the
yaml or draw them from a tabular file.

``` yaml
- last_name: Ganon
  first_name: Ri
  middle_name: ~
  role_type: creator
  email: ~
  orcid: ~
  data_source: "path/file.csv"
- last_name: Carrol
  first_name: Pete
  middle_name: ~
  role_type: creator
  email: pcarroll@seahawks.nfl
  orcid: 2222-2x22-2222-2222
  data_source: ~
- last_name: Payt
  first_name: Se
  middle_name: ~
  role_type: 
  email: ~
  orcid: ~
  data_source: "path/file.csv"
- last_name: Harbaugh
  first_name: Jim
  middle_name: ~
  role_type: associatedParty
  project_role: "head coach"
  email: ~
  orcid: ~
  data_source: "path/file.csv"
```

If employing a tabular csv file to generate personnel metadata, it must
have the following structure:

| last_name | first_name | middle_name | organization | email | orcid |
|----|----|----|----|----|----|
| Gannon | Richard | NA | Phoenix Cardinals | rgannon@cardinals.usfl | 1111-1111-11x1-1111 |
| Payton | Sean | NA | Colorado Broncos | spayton@broncos.usfl | NA |
| Harbaugh | Jim | NA | California Chargers | jharbaugh@chargers.usfl | 3x33-3333-3333-2222 |

#### data objects

**overview: create a EML dataTable**

There are (up to) three resources that we use to provide metadata about
our EML dataTable data objects. The workflow goes like this:

1.  Load the data into the R environment and process as appropriate.

2.  Generate a yaml template specific to that data object to document
    entity attributes.

`write_attributes(data_entity)` will generate a template as a yaml file
in the working directory based on properties of the data entity such
that metadata properties (e.g., attributeDefinition, units, annotations)
can be added via a editor.

3.  If relevant, generate a yaml template specific to that data object
    to document entity attributes that are factors (categorical).

`write_factors(data_entity)` will generate a template as a yaml file in
the working directory based on columns of the data entity that are
factors such that details of factor levels can be added via a editor.

4.  Add the data entity details (e.g., data object name, description) to
    the `data_objects.yaml` file in the project directory. An entry for
    a dataTable where the data object in the R environment is titled
    `datasonde_record` might look like the following:

``` yaml
datasonde_record:
  type: table
  dfname: datasonde_record
  description: "record of datasonde readings in the Tempe Town Lake, Tempe, Arizona, USA"
  dateRangeField: ~
  overwrite: TRUE
  projectNaming: TRUE
  missingValueCode: ~
  additional_information: ~
```

5.  when the dataset is created, any numeric attributes that had custom
    (i.e., not in the EML schemas) will be listed in a
    `custom_units.yaml` template file where a description can be
    provided.

*A special case of updating existing datasets:*

A common need with long-term, ongoing research to update existing
metadata. A challenge is that we do not want to have to rebuild from
scratch the attribute metadata for a data entity that we constructed
with `write_attributes()` at each update. In terms of attribute
metadata, definitions, units, etc. are relatively static but what often
change are the minimum and maximum values for numeric variables as the
observation record grows. We could ascertain the minimum and maximum
values for numeric variables then manually update existing attribute
metadata but this is tedious, error-prone, and can be time consuming
when dealing with many variables. The `update_attributes` function takes
care of this for us by reading the existing attribute metadata for a
given data entity and updating those metadata with the minimum and
maximum values for any numeric variables for said data entity.

Under the hood, `capeml` is using the `create_dataTable` function to
build the dataTable metadata in EML format for each tabular data
resource listed in `data_objects.yaml`. This function provides many
services for given a rectangular data matrix of type dataframe or tibble
in the R environment:

- the data entity is written to file as a csv in the working directory
  with the file name: identifier_data-entity-name.csv (or
  data-entity-name.csv if project naming is not invoked).
- metadata provided in the attributes and factors (if relevant)
  templates are ingested
- a EML object of type dataTable that reflects metadata detailed in the
  attributes and factors files noted above is returned
- units that are outside the EML standard unit library (e.g., custom,
  QUDT) are added to a `custom_units.yaml` file in the project directory

We can invoke `create_dataTable` outside of building a dataset, which
can be helpful for previewing dataTable EML metadata before it goes into
a xml file or debugging. A workflow around `create_dataTable` might look
like this:

``` r
my_table <- import / generate...process...

# Note: the `try` block facilitates knitting the entire document even if the
# attributes and factors yaml files already exist since they will not be
# overwritten unless the overwrite flag is set, thus aborting the knit.

try({
  capeml::write_attributes(my_table, overwrite = FALSE)
  capeml::write_factors(my_table, overwrite = FALSE)
})

my_table_desc <- "description of the table"

# create_dataTable() accepts additionalInfo but is not required

my_additional_info <- "more metadata""

my_table_DT <- capeml::create_dataTable(
  dfname                  = my_table,
  description             = my_table_desc,
  dateRangeField          = "my_date_field",
  additional_information  = my_additional_info
)
```

**overview: create a EML otherEntity**

A EML object of type otherEntity can be created from a single file or a
directory. In the case of generating a otherEntity object from a
directory, pass the directory path to the target_file_or_directory
argument, capeml will recognize the target as a directory, and create a
zipped file of the identified directory.

If the otherEntity object already is a zip file with the desired name,
set the overwrite argument to FALSE to prevent overwriting the existing
object.

As with all objects created with the capeml package, the resulting
object is named with convention: projectid_object-name.file extension by
default but this functionality can be turned off by setting
projectNaming to FALSE.

As with `create_dataTable()`, `create_otherEntity()` can also take
advantage of the `write_attributes()` and `write_factors()` services of
capeml. An example of where you might want to use these features would
be when documenting a spatial resource that cannot be documented as type
`spatialRaster` or `spatialVector` (e.g., because the resource is
projected in a coordinate reference system that is not part of the EML
schema). To use these services with a directory, create an object in R
with the same name as the directory that will be zipped, then pass that
object to `write_attributes()` and `write_factors()` - capeml will look
for the resulting attribute and factor (if relevant) yaml files and
match them to the directory name (see following for an example).

*example: create a EML otherEntity for a vector data object*

In this example, we will generate EML otherEntity metadata for a ESRI
shapefile titled UEI_Features_CAPLTER_2010_2017_JAB.shp (plus *.dbf,
*.prj, and other shapefile files) that is in a directory of the same
name.

``` r
# Read the data into R, here a shapefile using the sf package being careful to
# name the resulting object in the R environment with the same name of the
# directory housing the shapefiles (i.e., UEI_Features_CAPLTER_2010_2017_JAB).

UEI_Features_CAPLTER_2010_2017_JAB <- sf::st_read(
  dsn   = "/path/UEI_Features_CAPLTER_2010_2017_JAB/",
  layer = "UEI_Features_CAPLTER_2010_2017_JAB"
)

# add factors if and as appropriate

UEI_Features_CAPLTER_2010_2017_JAB <- UEI_Features_CAPLTER_2010_2017_JAB |>
  dplyr::mutate(UEI_type = as.factor(UEI_type))

# Generate yaml files of both the attributes and factors (if relevant) from the
# shapefile that we read into R; these will be written to the project directory
# with the name of the object that we created in the R environment in the first
# step - again, this must correspond to the name of directory housing the files
# to be zipped.

capeml::write_attributes(UEI_Features_CAPLTER_2010_2017_JAB, overwrite = TRUE)
capeml::write_factors(UEI_Features_CAPLTER_2010_2017_JAB, overwrite = TRUE)
```

As with a dataTable, we add the otherEntity details to the
`data_objects.yaml` file.

``` yaml
UEI_Features_CAPLTER_2010_2017_JAB:
  type: other
  target_file_or_directory: UEI_Features_CAPLTER_2010_2017_JAB
  description: "compilation of pre-existing..."
  overwrite: FALSE
  projectNaming: FALSE
  additional_information: "This is a spatial data object..."
```

As with `create_dataTable`, we can call `create_otherEntity` outside of
`data_objects.yaml` for previewing and debugging:

``` r
uei_features_other <- capeml::create_otherEntity(
  target_file_or_directory = "data/UEI_Features_CAPLTER_2010_2017_JAB",
  description              = "compilation of pre-existing..."
  additional_information   = "This is a spatial data object..."
)
```

*example create a EML otherEntity for a raster data object*

If the raster data are not categorical, we can simply pass raster value
details to the `entity_value_description` parameter and add the raster
file details to the `data_objects.yaml.`

``` yaml
well_water_use:
  type: other
  target_file_or_directory: "well_water_use.img"
  description: "Change of groundwater usage..."
  overwrite: FALSE
  projectNaming: FALSE
  additional_information: "This is a spatial data object..."
  entity_value_description: "acre-feet"
```

If the raster data are categorical, we can construct a template to
provide metadata about the factor levels using the
`write_raster_factors()` tool from the capemlGIS package.
`write_raster_factors()` works similarly to capeml’s `write_factors()`
but accommodates the matrix structure and single data type of raster
data. In the example below, the well_water_use raster features changes
in water level - the changes are in units of acre-feet but the changes
are binned in ranges such that the values are categorical. We can use
the `capemlGIS::write_raster_factors` function to generate a metadata
template (well_water_use.yaml) in the working directory that we can use
do document the details of the categories, which will be read when the
otherEntity EML is generated.

``` r
well_water_use <- read raster data "well_water_use.img"

capemlGIS::write_raster_factors(
  raster_entity = well_water_use,
  value_name    = "acre-feet"
)
```

``` yaml
well_water_use:
  type: other
  target_file_or_directory: "well_water_use.img"
  description: "Change of groundwater usage..."
  overwrite: FALSE
  projectNaming: FALSE
  additional_information: "This is a spatial data object..."
  entity_value_description: ~
```

**annotations**

`capeml` supports adding semantic annotations to attributes. This is
facilitated by adding *propertyURI*, *propertyLabel*, *valueURI*, and
*valueLabel* details to the `_attrs.yaml` file for a data object.
*Example, add semantic annotation (and other) metadata to the datetime
field of a data object…*

``` yaml
datetime:
  attributeName: datetime
  attributeDefinition: 'date and time (UTC-7) of data capture'
  propertyURI: 'http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#containsMeasurementsOfType'
  propertyLabel: 'contains measurements of type'
  valueURI: 'http://purl.dataone.org/odo/ECSO_00002043'
  valueLabel: 'date and time of measurement'
  columnClasses: Date
  formatString: YYYY-MM-DD
```

**units**

`capeml` supports the following unit types: (1) units in the EML
standard library, (2) custom units, and (3) units documented by QUDT.
QUDT is the preferred form of units, and the example below for the
*Temp_deg_C* variable illustrates adding Celsius unit metadata.

``` yaml
Temp_deg_C:
  attributeName: Temp_deg_C
  attributeDefinition: 'temperature as measured by the sensor'
  propertyURI: ~
  propertyLabel: ~
  valueURI: ~
  valueLabel: ~
  unit: 'DEG_C'
  numberType: real
  minimum: 0.0
  maximum: 44.88
  columnClasses: numeric
```

Both custom and QUDT units are documented in a `custom_units.yaml` file
that is written when the EML dataset is generated. In the case of QUDT
units, they are listed only for schema compliance. For custom units,
however, there is a description field for each custom units in
`custom_units.yaml` where a description should be provided.

In the case of QUDT units, these are documented also in a
`annotations.yaml` file that is read when the EML eml is generated (this
file does not need to be edited).

#### citations

Below are sample work flows that use `capeml`’s `create_citation`
function to generate citations by passing a resource DOI to crossref.
Citations can be added to EML `literatureCited` and `usageCitation`
elements. The work flow capitalizes on EML version 2.2 that accepts the
BibTex format for references.

`create_dataset()` will look for citation entities at the time of
dataset construction so desired citation entities must exist in the R
environment. `literatureCited` entities must be in a list named
`citations`, and `usageCitation` entities must be a list named `usages`.

Note that, unlike a `literatureCited` citation, a `usageCitation` is
**not** wrapped in a citation tag.

**literature cited**

``` r
cook    <- capeml::create_citation("https://doi.org/10.1016/j.envpol.2018.04.013")
sartory <- capeml::create_citation("https://doi.org/10.1007/BF00031869")

citations <- list(
  citation = list(
    cook,
    sartory
  ) # close list of citations
) # close citation
```

**usage citations**

``` r
brown <- capeml::create_citation("https://doi.org/10.3389/fevo.2020.569730")

usages <- list(brown) # close usages

dataset$usageCitation <- usages
```

**citations that do not have a DOI**

Though a DOI makes documenting references easy, we can add citations
that do not have a DOI. There are many ways to address this but likely
easiest is to get or create a citation for the reference in bibtex
format. [bibutils](http://sourceforge.net/p/bibutils/home/Bibutils/) is
a helpful utility that can convert other citation formats, such as .ris,
to bibtex. With bibutils, we can convert ris to an intermediate xml
format and then to bibtex.

``` sh
wget -O ~/Desktop/tellman_dissertation.ris https://repository.asu.edu/items/53734.ris
cat tellman_dissertation.ris | ris2xml | xml2bib >> tellman_dissertation.bib
```

Once we have the citation in bibtex format, we can add it along with
other citations as in the example below where we added the citation for
the Tellman dissertation to a suite of citations generated with capeml’s
`create_citiation` function.

``` r
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
)
```
