# Generate geographic coverage element(s) for one or more locations from a simple features object

**Deprecated.** This function has moved to the `capemlVector` package.
Please install `capemlVector` and call
`capemlVector::create_geographic_coverage()` instead.

## Usage

``` r
create_geographic_coverage(sf_object, description)
```

## Arguments

- sf_object:

  ignored

- description:

  ignored

## Value

Stops with a deprecation error.

## Note

This function was relocated to the `capemlVector` package to allow the
`capeml` package to remove its dependency on `sf`. Install the
replacement with: `pak::pak("CAPLTER/capemlVector")`
