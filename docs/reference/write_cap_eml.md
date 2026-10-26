# write eml to file

A wrapper around EML::write_eml that automates file naming from projet
configuration

## Usage

``` r
write_cap_eml(
      emlEntity,
      filePathName
    )
```

## Arguments

- emlEntity:

  (unquoted character) Unquoted name of the eml entity (from
  \`create_eml\`) to be written to file. Defaults to \`eml\`.

- filePathName:

  (character) Path and file name of file to be written. Defaults to the
  package identifier and current directory.

## Value

Writes eml object as specified name and to specified location in xml
format.

## Details

Any existing xml files named with `packageScopeNumber` at `path` will be
overwritten.

## Note

When `filePathName` is not provided, the package version is read from
the optional `version` key in config.yaml if present; otherwise it is
determined from the EDI repository with
[`get_next_version`](get_next_version.md), which requires the
`EDI_API_KEY` environment variable and network access to EDI. See
[`read_package_configuration`](read_package_configuration.md).

## Examples

``` r
if (FALSE) { # \dontrun{

write_cap_eml(
  emlEntity = myeml,
  filePathName = "~/Desktop/myeml.xml"
)

write_cap_eml()

} # }
```
