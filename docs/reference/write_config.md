# write a project config.yaml file

write_config writes a template config.yaml file to the working directory
(default) or supplied path.

## Usage

``` r
write_config(
  scope = "knb-lter-cap",
  identifier,
  version = NULL,
  path = ".",
  overwrite = FALSE
)
```

## Arguments

- scope:

  (character) Quoted name of the package scope (e.g., "edi"). The
  default is "knb-lter-cap".

- identifier:

  (integer) Data package identifier (number).

- version:

  (integer) Optional data package version to hard-code in config.yaml.
  Defaults to `NULL` (version determined from the EDI repository).

- path:

  (character) Path to where the config file will be written. Defaults to
  the current directory.

- overwrite:

  (logical) Logical indicating if an existing config file in the target
  directory should be overwritten.

## Details

A template config.yaml file is written to the working directory or
specified location. The function requires that the package scope and
number (e.g., "edi", 521) are provided. By default, no package version
is written and capeml determines the next version from the EDI
repository. Supplying `version` hard-codes the version in config.yaml so
that no EDI API call is needed to determine it (see
[`read_package_configuration`](read_package_configuration.md)).

Two optional keys may be added to config.yaml to identify data and
metadata that do not reside in the working directory (see
[`get_locations`](get_locations.md)): `data_path`, one or more
directories that are searched (recursively, if needed) for data files;
and `metadata_path`, a directory to which attribute and factor templates
are written and from which they are read. The working directory is used
if these keys are not provided.

## Note

The function expects specifically a 3-digit package identifier (number).
