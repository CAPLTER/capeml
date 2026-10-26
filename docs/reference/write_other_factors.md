# Create a template yaml file for supplying factor metadata for a data object that cannot be read into R

`write_other_factors` creates a template as a yaml file for supplying
the codes and definitions of categorical attributes of an otherEntity
(e.g., a zip file of LiDAR point clouds in LAZ format) that cannot be
read into the R environment.

## Usage

``` r
write_other_factors(
  target_file,
  factors,
  overwrite = FALSE,
  metadata_path = NULL
)
```

## Arguments

- target_file:

  (character) The quoted name of the data file (e.g.,
  "lidar_tiles.zip"). The file is used only to name the template; it
  does not need to be readable.

- factors:

  (list) A named list in which each name is a categorical attribute and
  each element is a character vector of the codes of that attribute; use
  `character(0)` if the codes are not yet known.

- overwrite:

  (logical) Logical indicating if an existing factors file in the target
  directory should be overwritten.

- metadata_path:

  (character) Directory to which the template is written. Defaults to
  `metadata_path` in config.yaml, else the working directory.

## Value

The path to the template is returned invisibly, and a template for
providing code definitions is written to `metadata_path`.

## Details

The template is named for the target file without its extension (e.g.,
lidar_tiles.zip yields lidar_tiles_factors.yaml) and written to
`metadata_path`. Each attribute of class factor in the attributes
template (see [`write_other_attributes`](write_other_attributes.md))
must have an entry in the factors template, and every code requires a
definition. Codes that are not known when the template is created are
written as a single blank level to be completed (add further levels as
needed). If a factors template is provided without an attributes
template, `create_otherEntity` documents a single attribute using the
`entity_value_description` argument (e.g., the value classes of a
raster).

## When to use

Attributes describe variables or fields that exist within the data
object, such as the point record fields (e.g., X, Y, Z, Classification)
of LiDAR point clouds in a zip file, the attribute table of a zipped
shapefile, the value classes of a raster, or the columns of spreadsheets
in a zip file. Each attribute must correspond to a variable in the
object.

## What is not allowed

Attributes are not a place for metadata about the file itself (e.g.,
author, number of files in an archive, version, software used to create
it); provide those details in the `description` or
`additional_information` arguments of `create_otherEntity`, the methods,
or dataset-level metadata. Do not invent attributes that do not exist in
the object.

## Examples

``` r
if (FALSE) { # \dontrun{

 # LAS classification codes of the LiDAR point clouds (LAZ) in a zip file;
 # use character(0) for a factor whose codes are not yet known

 capeml::write_other_factors(
   target_file = "lidar_tiles.zip",
   factors     = list(
     Classification = c("1", "2", "3", "4", "5", "6", "7", "17")
   )
 )

} # }
```
