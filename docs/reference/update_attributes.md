# Update minimum and maximum values of numeric variables in existing attribute metadata

A frequent need with long-term, ongoing research is to update existing
data. A challenge to that is that we do not want to have to rebuild from
scratch the attribute metadata for a data entity at each update. In
terms of attribute metadata, definitions, units, etc. are relatively
static but what can change are the minimum and maximum values for
numeric variables as the observation record grows. We could ascertain
the minimum and maximum values for numeric variables then manually
update existing attribute metadata but this is tedious, error-prone, and
can be time consuming when dealing with many variables. The
`update_attributes` function takes care of this for us by reading the
existing attribute metadata for a given data entity and updating those
metadata with the minimum and maximum values for said data entity if
they have changed in the context of a data refresh.

## Usage

``` r
update_attributes(entity_name, return_type = "yaml", metadata_path = NULL)
```

## Arguments

- entity_name:

  (character) The name of the data entity.

- return_type:

  (character) Quoted designator indicating the value returned as either
  a attributes template yaml file (return_type = "yaml", the default) or
  a list of entity attributes (return_type = "attributes") constructed
  from the data entity. The latter (i.e., return_type = "attributes") is
  really just for testing the function.

- metadata_path:

  (character) Directory of metadata templates. Defaults to
  `metadata_path` in config.yaml, else the working directory.

## Value

An updated metadata template including refreshed minimum and maximum
vales for numeric variables for providing attribute metadata as a yaml
file with the file name of the R data object + "\_attrs.yaml" in the
directory where the existing attributes file was found.

## Note

An artifact of the updating process is that empty/unused keys will be
omitted from the updated yaml file. For example, empty annotation fields
will not be present in the yaml file after the update. This does not
affect functionality of the metadata generated (the keys were not used
anyway) but the keys would have to be added manually if there was a need
for them in the future.

An artifact of the updating process is that the `definition` element is
populated in the updated yaml file. The `definition` element is required
by the EML schema for attributes of type character. In a typical
workflow, the `definition` element is left blank in the metadata yaml
file and is automatically populated with a copy of the
`attributeDefinition` element at build time (e.g., `create_dataTable`).
Because `update_attributes` is calling `read_attributes`, which is what
populates any empty `definition`s for variables of type character, this
is reflected by `update_attributes` in the updated yaml. This does not
have any bearing on the resulting data entity EML, and we can still
provide custom `definition` metadata if desired.

`update_attributes` will abort the update if an attribute is detected in
the data entity but for which there is not metadata in the existing
attributes file. This is an indication that the data structure or
content has changed sufficiently that a new, blank attributes metadata
file should be constructed. Conversely, if an attribute is detected in
the existing metadata that is not detected in the data entity, the
update will proceed but the attribute and corresponding metadata in the
entity but not in the existing metadata file will be stricken from the
updated attribute metadata file. In both cases, `update_attributes` will
print to screen the incongruent attributes.

## Examples

``` r
if (FALSE) { # \dontrun{

 # update attributes file for mycars data object

 mycars <- head(mtcars)

 capeml::update_attributes(entity_name = mycars)

} # }
```
