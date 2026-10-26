# Load attribute information from a entity attribute yaml or csv file

The `read_attributes_file` function reads an entity's attribute details
from a "entity name"\_attrs.yaml or "entity name"\_attrs.csv file in
`metadata_path` or the working directory - the details of which are
incorporated into the EML metadata for that entity.

## Usage

``` r
read_attributes_file(
  string_pointer,
  entity_id,
  metadata_path = NULL,
  also = NULL
)
```

## Arguments

- string_pointer:

  (character) The quoted name of the data entity.

- entity_id:

  (character) Quoted identifier of the data object that is being
  described, this will usually be the name or hash of the data table (or
  otherwise) of which the attribute is associated.

- metadata_path:

  (character) Directory of metadata templates. Defaults to
  `metadata_path` in config.yaml, else the working directory.

- also:

  (character) Additional directories in which to look for the attributes
  file (e.g., the directory of a data file for templates co-located with
  the data).

## Value

A list of a list of attributes and column classes

## Details

The `read_attributes_file` function reads an entity's attribute details
from a "entity name"\_attrs.yaml or "entity name"\_attrs.csv file in
`metadata_path` or the working directory - the details of which are
incorporated into the EML metadata for that entity.

## Note

Recent versions of the capeml package generate attribute (and factor)
metadata files in yaml format; the `read_attributes_file` function will
look also for attributes files in csv format to accommodate older
projects.

The `read_attributes_file` function is intended primarily as a helper to
other functions in the capeml ecosystem (notably `read_attributes` and
`read_raster_attributes`) so is not meant to be called directly (but can
be).
