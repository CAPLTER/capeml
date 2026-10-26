# create a EML entity of type dataset

`create_dataset` generates a EML entity of type dataset.

## Usage

``` r
create_dataset(publication_date = NULL)
```

## Arguments

- publication_date:

  (character) Quoted ISO date - defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

## Value

EML entity of type dataset entity is returned.

## Details

A dataset entity, the central component of a data package, is created
from objects in the user's R environment and supporting yaml files.
Prior to processing, any existing annotations.yaml in the working
directory is removed silently to avoid stale unit-annotation references.
Existing custom_units.yaml content is preserved because it may contain
investigator- authored descriptions that are merged forward by unit
name. A project (default is LTER) indicates contact and project details
specific to the research. The abstract and methods must be in markdown
format - package will look for these files (abstract.md, methods.md) in
the project directory. Similarly for keywords, the package will look for
a keywords.csv file in the project directory.

## Note

`create_dataset` will look for most inputs used to construct a dataset,
such as a package number, in the working environment or from the project
config.yaml; these parameters are not passed directly to the function
and must exist in the working environment or configuration file(s).

The package version is read from the optional `version` key in
config.yaml if present; otherwise it is determined from the EDI
repository with [`get_next_version`](get_next_version.md), which
requires the `EDI_API_KEY` environment variable and network access to
EDI. See [`read_package_configuration`](read_package_configuration.md).
