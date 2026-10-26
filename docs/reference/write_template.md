# Construct a template capeml workflow

Write a workflow template qmd file for the capeml workflow

## Usage

``` r
write_template(
  scope = "knb-lter-cap",
  identifier,
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

- path:

  (character) Path to where the config file will be written. Defaults to
  the current directory.

- overwrite:

  (logical) indicates to overwrite an existing file if one exists
  (default = FALSE).

## Value

A quarto (qmd) file named with the project scope and identifier
(number).

## Details

Existing files with the package name and .qmd extension at `path` will
not be overwritten unless overwrite is made explicit.

## Note

While the function does provide a generalized template for much of the
capeml workflow, creating data entities (e.g., dataTable, otherEntity)
are not included. Further, the template is geared heavily to drawing on
a metadata
\[template\](https://globalfutures.asu.edu/caplter/data/submit-data/)
that is part of the data submission process for the CAP LTER.

## Examples

``` r
if (FALSE) { # \dontrun{
# Template data package directory "edi.521"

capeml::write_template(
  scope      = "edi",
  identifier = 521,
  path       = '~/Desktop'
)

} # }
```
