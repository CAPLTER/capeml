# Construct a csv tabular data file and create metadata of type dataTable

`create_dataTable` writes a tabular data file of type csv and generates
a EML entity of type dataTable

## Usage

``` r
create_dataTable(
  dfname,
  description,
  dateRangeField = NULL,
  overwrite = FALSE,
  projectNaming = TRUE,
  missingValueCode = NULL,
  additional_information = NULL,
  metadata_path = NULL
)
```

## Arguments

- dfname:

  (character) The quoted or unquoted name of the R data frame or tibble.

- description:

  (character) A quoted description of the data frame or tibble that will
  populate the entityDescription element of the EML document. name of
  the raster data to be processed is included as a parameter in this
  metadata file).

- dateRangeField:

  (optional) (character) The quoted name of the data entity field that
  is a date field that would reflect the start and end dates of the data
  reflected in the data entity.

- overwrite:

  (logical) A logical indicating whether to overwrite an existing file
  bearing the same name as csv file as it is imported.

- projectNaming:

  (logical) Logical indicating if the resulting csv file should be
  renamed per the style used in the capeml ecosystem with the project
  id + base file name + file extension. If set to false, the resulting
  csv will bear the name the object in the R environment.

- missingValueCode:

  (optional) (character) `create_dataTable` will automatically document
  the presence of NA and NaN entries as missing values in the EML
  output. The user has the ability to identify one additional indicator
  of missing values (e.g., "-9999", "missing").

- additional_information:

  (character) Additional information about the data object. Should be
  quoted, and accepts (but does not require) markdown formatting.

- metadata_path:

  (character) Directory of metadata templates. Defaults to
  `metadata_path` in config.yaml, else the working directory. The csv
  file is always written to the working directory.

## Value

EML dataTable object is returned. Additionally, the data entity is
written to file as type csv, and renamed with the package number + base
file name + md5sum + file extension (csv in this case).

## Details

`create_dataTable` writes a tabular data file of type csv and creates a
corresponding EML dataTable object from a data frame or tibble object in
the R environment. The function reads the attributes and classes
contained within a supporting yaml file generated from
`write_attributes` function - `create_dataTable` will look for a file in
`metadata_path` or the working directory with a name of type
data_object_name_attrs.yaml. Factors also are read from a supporting
yaml file - `create_dataTable` will look for a file in `metadata_path`
or the working directory with a name of type
data_object_name_factors.yaml. If that exists, the factor details
outlined in that file will be incorporated into the EML, else the EML
will be built without factor metadata. Note that this functionality is
predicated on the existence of a file containing metadata about any
factors in the dataframe, that the file is in `metadata_path` or the
working directory, and that the file matches the dataframe name
precisely; the function does not look for variables of type factor in
the dataframe. In addition to generating a EML entity of type dataTable,
`create_dataTable` writes the R object to file as type csv, and renames
the file with package number + base object name + file extension (csv in
this case) if selected.

## Note

If project naming is TRUE then `create_dataTable` will look for a
package number (packageNum) in \`config.yaml\`; this parameter is not
passed to the function and it must exist.

## Examples

``` r
if (FALSE) { # \dontrun{

mycars <- mtcars |>
  head()

try({
  capeml::write_attributes(mycars)
  capeml::write_factors(mycars)
})

# create_dataTable with optional arguments dateRangeField and missingValueCode
mycars_DT <- capeml::create_dataTable(
 dfname      = mycars,
 description = "just a few cars",
 )

} # }
```
