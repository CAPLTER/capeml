# create a EML entity of type EML

`create_eml` generates a EML entity of type eml from objects and
metadata compiled from capeml objects in the working directory

## Usage

``` r
create_eml()
```

## Value

EML entity of type EML is returned.

## Details

A eml entity is created from objects defined in the user's R environment
and configuration files.

## Note

`create_eml` will look for most inputs used to construct a eml entity,
such as access and dataset, in the working environment; these parameters
are not passed directly to the function and must exist in the working
environment.

Some parameters, such access, are loaded in the background when the '

The package version is read from the optional `version` key in
config.yaml if present; otherwise it is determined from the EDI
repository with [`get_next_version`](get_next_version.md), which
requires the `EDI_API_KEY` environment variable and network access to
EDI. See [`read_package_configuration`](read_package_configuration.md).
