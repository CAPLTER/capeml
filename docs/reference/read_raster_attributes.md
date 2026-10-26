# Read raster attribute information from attributes metadata file

The `read_raster_attributes` function reads a raster's attribute details
from a "entity name"\_attrs.yaml or "entity name"\_attrs.csv file in
`metadata_path` or the working directory - the details of which are
incorporated into the EML metadata for that entity.

The `read_raster_attributes` function reads a raster's attribute details
from a "entity name"\_attrs.yaml or "entity name"\_attrs.csv file in
`metadata_path` or the working directory - the details of which are
incorporated into the EML metadata for that entity.

## Usage

``` r
read_raster_attributes(
  entity_name,
  entity_id = "data_entity",
  metadata_path = NULL
)
```

## Arguments

- entity_name:

  (character) The quoted name of the data entity.

- entity_id:

  (character) Quoted identifier of the data object that is being
  described, this will usually be the name or hash of the data table (or
  otherwise) of which the attribute is associated.

- metadata_path:

  (character) Directory of metadata templates. Defaults to
  `metadata_path` in config.yaml, else the working directory.

## Value

A entity of type EML attributes or list of attributes (for testing and
debugging)

## Note

The `read_raster_attributes` function is intended primarily as a helper
to other functions in the capeml ecosystem so is not meant to be called
directly (but can be).

The `read_raster_attributes` function is a helper function to the
capemlGIS package. This separate function for attributes associated with
rasters (as opposed to any other data type with the `read_attributes`
function) is to avoid the tricky challenge of reading the data object
into the R environment that is integral to `read_attributes`; in some
cases we do read the raster object into the environment but not always
so we do not want to force it as would be the case with
`read_attributes`.
