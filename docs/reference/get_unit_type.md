# get the reference to a unit

`get_unit_type` will determine the reference and details (if relevant)
of a supplied string as either qudt_en_us, qudt_en_uk, custom, or from
the EML standard dictionary. The latter returns `NULL`.

## Usage

``` r
get_unit_type(this_unit)
```

## Arguments

- this_unit:

  (character) Quoted string of unit

## Value

NULL if the provided string matches a reference in the EML standard unit
dictionary; a tibble of unit details if in the QUDT unit dictionary, or
a tibble of name and type fields if a custom unit (i.e., not in the EML
standard unit dictionary or the QUDT standard unit dicationary).

## Details

The unit referece is determined by comparing the provided string against
catalogues of the EML standard unit dictionary, and subsets of the QUDT
unit dictionary. The QUDT unit dictionary is segregated between units in
the english US and UK variants. `get_unit_type` will try first to match
the supplied string against units in the QUDT subset of enlish (US)
units and seek a match in the UK variant if a US match is not found.
Units that are in the EML standard library return `NULL` value as
further documentation is not required for that set. `get_unit_type` is
designed as a helper function for other tools within `capeml` that
involve units but can be used independently.

## Examples

``` r
if (FALSE) { # \dontrun{

capeml::get_unit_type("dog_years")
#        name   type
# 1 dog_years custom

capeml::get_unit_type("C-M")
# A tibble: 1 × 4
# name    unit                           label         type      
# <chr>   <chr>                          <chr>         <chr>     
# 1 C-M   http://qudt.org/vocab/unit/C-M Coulomb Meter qudt_en_us

capeml::get_unit_type("meter")
# NULL

} # }
#'
```
