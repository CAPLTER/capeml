# Create the taxonomicCoverage EML node

Create the taxonomicCoverage EML node

## Usage

``` r
set_taxonomic_coverage(sci_names)
```

## Arguments

- sci_names:

  (list) Object returned by
  [`capeml::create_taxa_list()`](create_taxa_list.md).

## Value

- list:

  a emld list object is returned for use with the EML R Package

- .xml file:

  a .xml file is written to the working directory

## Note

courtesy of the taxonomyCleanr package
