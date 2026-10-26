# Configure LTREB project details for a EML project entity

configure_ltreb_project establishes an EML::project with the elements:
(1) personnel, (2) project abstract, and (3) funding (grant numbers).

## Usage

``` r
configure_ltreb_project()
```

## Value

establishes in the R environment upon package load a EML::project entity
specific to the Sycamore Creek LTREB. If appropriate to include in a
dataset, 'ltrebProject' should be referenced with the project argument
when constructing a EML::dataset.

## Details

configure_ltreb_project establishes an EML::project with the elements:
(1) personnel, (2) project abstract, and (3) funding (grant numbers).
