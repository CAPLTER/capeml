# create or update data package with EDI API

`create_package` creates or updates a data package in the EDI data
repository.

## Usage

``` r
create_package(
  identifier = NULL,
  scope = NULL,
  version = NULL,
  environment = "production",
  update = FALSE
)
```

## Arguments

- identifier:

  (integer) Unquoted package identifier integer

- scope:

  (character) Quoted package scope (e.g., "knb-lter-cap", "edi")

- version:

  (integer) Unquoted package version integer. If omitted,
  \`create_package()\` will determine the next version with
  \`get_next_version()\`. If supplied, that value is used directly and
  automatic EDI login is skipped.

- environment:

  (character) Quoted name of the EDI environment where the data package
  should be created, can be: production (default), staging, or
  development.

- update:

  (logical) Boolean value indicating if the data package already exists
  and should be updated.

## Note

`create_package` will look for a config.yaml file to estalish the
package scope and identifier. Alternatively, these parameters can be
provided by the user but, in that case, both parameters must be
provided.

\`create_package()\` uses the EDI API key stored in the \`EDI_API_KEY\`
environment variable. If \`version\` is supplied manually, the automatic
\`edi_login()\` and version lookup steps are skipped, but EDI still
requires the API key for package creation or update.
