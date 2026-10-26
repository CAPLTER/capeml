# Read information from project configuration file ("config.yaml")

The read_package_configuration function reads the project configuration
details from the config.yaml file that must exist in the working
directory. This is a internal, convenience function intended mostly for
the utility of other capeml functions but read_package_configuration can
be run independenty.

## Usage

``` r
read_package_configuration()
```

## Value

A list of configuration details. The `version` element is an integer
when `version` is set in config.yaml and `NULL` otherwise.

## Details

An optional `version` key in config.yaml hard-codes the data package
version (e.g., `version: 3`). When present, capeml functions that
construct the package name ([`create_dataset`](create_dataset.md),
[`create_eml`](create_eml.md), [`write_cap_eml`](write_cap_eml.md)) use
that value and do not query the EDI repository with
[`get_next_version`](get_next_version.md). This is useful where the EDI
API is slow or unavailable (e.g., on ASU SOL). The value must be a
positive whole number and is not updated automatically: increment it for
each new revision of the package. Omit the key to have capeml determine
the next version from the EDI repository.

## Note

This function if a bit of overkill in the sense that other capeml
functions (e.g., create_dataTable) could eaisily access the
configuration details by reading the config.yaml file. The purpose here
is mostly to accommodate the evolving structure of the config.yaml
structure. For example, capeml formerly took a projectIdentifier that
featured scope + dataset number + version. Logic is included here to
accommodate older and newer config.yaml styles; this logic is better
encapsulated into this single function rather than every place
configuration details are accessed.
