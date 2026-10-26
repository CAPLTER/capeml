# get the next version of a data package

`get_next_version` will access the EDI API to identify and return the
next version of a data package. If the data package does not exist
(i.e., we are constructing a new package), `get_next_version` will
return \`1\` and display a corresponding note.

## Usage

``` r
get_next_version(
  provided_scope = "knb-lter-cap",
  provided_identifier,
  display_message = FALSE
)
```

## Arguments

- provided_scope:

  (character) scope of data package (defaults to CAP LTER: knb-lter-cap)

- provided_identifier:

  (integer) identifier of data package

- display_message:

  (boolean) indicates whether to display a message if an existing
  dataset is not identifed in the repository (thus returning a version
  number 1)

## Value

integer reflecting the next version of a data package

## Details

\`get_next_version()\` uses the EDI API key stored in the
\`EDI_API_KEY\` environment variable.

`get_next_version` is a helper function designed to aid construction of
a package identifier (e.g., the version \`5\` of
\`knb-lter-cap.624.5\`). As such, the most common use case is that the
function will be called internally from `capeml::createDataset` but the
function can be called directly.

To avoid this API call, set the optional `version` key in config.yaml
(see [`read_package_configuration`](read_package_configuration.md));
capeml functions that construct the package name will then use that
value instead.

## Examples

``` r
if (FALSE) { # \dontrun{

get_next_version(
  provided_scope      = "knb-lter-cap",
  provided_identifier = 624
)

} # }
#'
```
