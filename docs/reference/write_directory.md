# construct campel supporting files in a data package directory

Create a directory structure for data package contents and `capeml`
files.

## Usage

``` r
write_directory(
      scope,
      identifier,
      path
    )
```

## Arguments

- scope:

  (character) Quoted name of the package scope (e.g., "edi"). The
  default is "knb-lter-cap".

- identifier:

  (integer) Data package identifier (number).

- path:

  (character) Path to where the config file will be written. Defaults to
  the current directory.

## Value

A directory with the following structure and contents:

- **name** Name supplied via `scope` and `identifier`

  - **config.yaml** configuration file

  - `data package name`.Rmd workflow template

## Details

Existing directories named with `data package name` at `path` will not
be overwritten.

## Examples

``` r
if (FALSE) { # \dontrun{
# Template data package directory "edi.521"

capeml::write_directory(
  scope      = "edi",
  identifier = 521,
  path       = '~/Desktop'
)

} # }
```
