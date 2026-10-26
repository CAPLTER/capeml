# Create a template yaml file for supplying attribute metadata for a data object that cannot be read into R

`write_other_attributes` creates a template as a yaml file for supplying
attribute metadata for an otherEntity (e.g., a zip file of LiDAR point
clouds in LAZ format) that cannot be read into the R environment. Unlike
`write_attributes`, which derives a template from a data object in R,
the names and classes of the attributes are provided by the user.

## Usage

``` r
write_other_attributes(
  target_file,
  attributes,
  column_classes = "character",
  overwrite = FALSE,
  metadata_path = NULL
)
```

## Arguments

- target_file:

  (character) The quoted name of the data file (e.g.,
  "lidar_tiles.zip"). The file is used only to name the template; it
  does not need to be readable.

- attributes:

  (character) The names of the attributes (variables) in the data
  object.

- column_classes:

  (character) The class of each attribute: numeric, character, factor,
  or Date. A single value applies to all attributes. Defaults to
  character.

- overwrite:

  (logical) Logical indicating if an existing attributes file in the
  target directory should be overwritten.

- metadata_path:

  (character) Directory to which the template is written. Defaults to
  `metadata_path` in config.yaml, else the working directory.

## Value

The path to the template is returned invisibly, and a template for
providing attribute metadata is written to `metadata_path`.

## Details

The template is named for the target file without its extension (e.g.,
lidar_tiles.zip yields lidar_tiles_attrs.yaml) and written to
`metadata_path`. `create_otherEntity` reads the annotated template and
adds the metadata to the attributeList of the resulting otherEntity. The
EML schema permits an attributeList for any otherEntity regardless of
file format.

## When to use

Attributes describe variables or fields that exist within the data
object, such as the point record fields (e.g., X, Y, Z, Classification)
of LiDAR point clouds in a zip file, the attribute table of a zipped
shapefile, the value classes of a raster, or the columns of spreadsheets
in a zip file. Each attribute must correspond to a variable in the
object.

## Required fields by column class

Every attribute requires an `attributeDefinition`. Additional fields
depend on `columnClasses`:

- numeric:

  `unit` (an EML standard unit, see
  [`EML::get_unitList()`](https://docs.ropensci.org/EML/reference/get_unitList.html),
  or a custom unit, which is documented in custom_units.yaml when the
  EML is built) and `numberType` (one of natural, whole, integer, real).
  `minimum` and `maximum` are optional and must be entered manually.

- character:

  no additional fields; leave `definition` blank as it is populated from
  `attributeDefinition`.

- factor:

  the codes and their definitions are provided in a factors template
  (see [`write_other_factors`](write_other_factors.md)).

- Date:

  `formatString` describing the format of the date or time (e.g.,
  YYYY-MM-DD, YYYY, hh:mm:ss).

The `propertyURI`, `propertyLabel`, `valueURI`, and `valueLabel` fields
are optional semantic annotations. `create_otherEntity` stops and lists
any missing required fields.

## What is not allowed

Attributes are not a place for metadata about the file itself (e.g.,
author, number of files in an archive, version, software used to create
it); provide those details in the `description` or
`additional_information` arguments of `create_otherEntity`, the methods,
or dataset-level metadata. Do not invent attributes that do not exist in
the object.

## Limitations

Because the data are not read, `minimum` and `maximum` cannot be
calculated, and missing value codes are not detected or supported in
these templates.

## Examples

``` r
if (FALSE) { # \dontrun{

 # point record fields of the LiDAR point clouds (LAZ) in a zip file

 capeml::write_other_attributes(
   target_file    = "lidar_tiles.zip",
   attributes     = c("X", "Y", "Z", "Classification", "GpsTime"),
   column_classes = c("numeric", "numeric", "numeric", "factor", "numeric")
 )

} # }
```
