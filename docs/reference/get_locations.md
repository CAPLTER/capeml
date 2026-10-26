# Resolve data and metadata locations for a project

`get_locations` reads the optional `data_path` and `metadata_path` keys
from config.yaml in the working directory.

## Usage

``` r
get_locations()
```

## Value

A list with elements `data_path` (character vector, NULL if not
configured) and `metadata_path` (character, "." if not configured).

## Details

Both keys are optional, as is config.yaml itself, so that projects in
which all data and metadata reside in the working directory work without
any configuration. `data_path` may be one or more directories that are
searched (recursively, if needed) for data files; `metadata_path` is a
single directory to which attribute and factor templates are written and
from which they are read. Relative paths are interpreted relative to the
working directory and a leading tilde is expanded.

## Examples

``` r
if (FALSE) { # \dontrun{

 # config.yaml
 # data_path:
 #   - /scratch/project/raw
 #   - /data/shared/lter
 # metadata_path: /scratch/project/metadata

 capeml::get_locations()

} # }
```
