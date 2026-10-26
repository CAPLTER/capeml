# translate custom and QUDT unit metadata to eml

`get_custom_units` harvests metadata pertaining to units of type custom
and QUDT detailed in \`custom_units.yaml\`. Custom units, i.e., those
not documented by QUDT or included in the EML standard libary, have a id
attribute and description element, where as QUDT units have only a id
attribute since their presence in the EML \<unitList\> is merely a
placeholder to accommodate EML congruence.

## Usage

``` r
get_custom_units()
```

## Value

EML eml entity is returned.
