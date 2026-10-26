# Create template yaml file(s) for supplying attribute unit metadata

`write_units` creates template yaml file(s) for supplying attribute QUDT
and custom unit metadata for a data objects that resides in the R
environment (or on disk in the case of rasters).

## Usage

``` r
write_units(
  entity_name = NULL,
  entity_id = NULL,
  raster = FALSE,
  provided_attributes_table = NULL,
  metadata_path = NULL
)
```

## Arguments

- entity_name:

  (character) Quoted name of the data object in the R environment

- entity_id:

  (character) A string that uniquely identifies the object in the R
  environment (e.g., the hash of a table)

- raster:

  (boolean) Boolean indicating that the target data are a raster

- provided_attributes_table:

  (tbl_df,tbl,data.frame) attributes table equivalent to
  attrs\[\["table"\]\]

- metadata_path:

  (character) Directory of metadata templates. Defaults to
  `metadata_path` in config.yaml, else the working directory.

## Value

\`annotations.yaml\`, \`custom_units.yaml\`, both, or neither depending
on the unit reference (QUDT, custom, EML standard unit library) used to
characterize attribute units for a data entity.

## Details

`write_units` generates zero to two yaml template files based on the
reference of attribute units associated with a data entity. A
\`annotations.yaml\` template is generated if attribute units are in the
QUDT unit dictionary. A \`custom_units.yaml\` template is generated for
custom (i.e., not in the EML standard units or QUDT dictionaries) and/or
QUDT units. In the case of custom units, entries include a description
field for investigators to provide a description of the unit. In the
case of QUDT units, only the name and label of the unit are included in
\`custom_units.yaml\` - QUDT metadata are documented in the
\<annotation\> element but a reference to any units not in the EML
standard unit list must be referenced in \<additionalMetadata\>.
\`annotations.yaml\` and \`custom_units.yaml\` will be created if both
QUDT and custom units are identified, and neither template will be
written if the data entity does not have units or if all of the units
are in the EML standard dictionary (because further documentation is not
required). Unit metadata will be appended to templates if they already
exist in the working directory.

## Note

`write_units` evalutes a data object's attributes metadata file (e.g.,
\`my_table_attrs.yaml\`). As such, this file must exist in
`metadata_path` or the working directory. This applies also to rasters
for which a my_raster.yaml is employed. Unit templates
(\`annotations.yaml\`, \`custom_units.yaml\`) are always written to the
working directory.

## Examples

``` r
if (FALSE) { # \dontrun{

 # create units metadata file(s) from the algae_attrs.yaml
 # file in the working directory

 algae <- read.csv("algae.csv")

 capeml::write_units(
   entity_name = "algae",
   entity_id   = tools::md5sum("algae.csv")
 )

} # }
```
