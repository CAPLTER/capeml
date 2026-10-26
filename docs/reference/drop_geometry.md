# Drop the geometry column from a simple features object

`drop_geometry` removes the active geometry column from a simple
features (sf) object and strips the sf class, returning a plain data
frame or tibble suitable for tabular-metadata workflows. If the input is
not an sf object it is returned unchanged.

## Usage

``` r
drop_geometry(x)
```

## Arguments

- x:

  An R object, typically a data frame, tibble, or simple features (sf)
  object.

## Value

If `x` inherits from class `"sf"`, returns `x` as a plain data frame or
tibble with the active geometry column removed and the `"sf"` class
dropped. Otherwise returns `x` unchanged.

## Note

`drop_geometry` is an internal helper used by `write_attributes`,
`write_factors`, and `read_attributes` to avoid a hard runtime
dependency on
[`sf::st_drop_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.html)
in those tabular workflows. It is not exported.

The geometry column is identified via the `sf_column` attribute of the
object, which is set by the sf package when the object is created. Only
that column is removed; all other columns are preserved.
