# Create taxonomicCoverage EML node from taxonomic data

This function reads a taxa data file in the working directory
(taxa_map.csv) and generates EML-compliant taxonomicClassification
node(s) suitable for use with the EML R package. It uses
[`create_taxa_list()`](create_taxa_list.md) to generate a list of
single-rank taxonomic classification records. The resulting EML node can
be incorporated as a taxonomicCoverage element as part of a EML
coverage. In addition to returning a list of taxonomicClassification(s),
the resulting content is written to `taxonomicCoverage.xml` in the
working directory.

## Usage

``` r
create_taxonomicCoverage()
```

## Value

A list EML-compliant `taxonomicClassification` nodes. The function also
writes `taxonomicCoverage.xml` to the working directory.

## Details

The input taxa data file (`taxa_map.csv`) must contain at least the
columns `taxonRank`, `taxa_clean` (the value for the specified rank),
`taxonID` (e.g., "ITIS:12345" or "GBIF:67890"), and optionally
`vernacularName` (common names, possibly comma- or semicolon-separated).

Only the rank specified in `taxonRank` is included for each record. The
function is robust to missing or empty `vernacularName` and `taxonID`
fields (these are omitted). The output is suitable for use in EML
metadata or other workflows requiring a flat, single-rank taxonomic
classification structure.

The name of the function is a bit misleading as it does not actually
return a taxonomicCoverage but rather a list of
taxonomicClassification(s) that are added to coverage as a
taxonomicCoverage sensu `coverage$taxonomicCoverage <- taxaCoverage`.
taxonomicCoverage is part of the title as that is the essence and
taxonomicClassification could have added a different confusion of
suggesting that it constructs only a single taxonomicClassification.

It is important to note that a taxonomic tree for each organism is not
generated. Rather, the function builds a \`taxonomicClassification\`
element for each taxon that features only the derived rank of the
organism. For example, if the taxon *Larrea tridentata* is submitted,
the output will include content (authority, authority ID, and common
name (if relevant)) for that species. That is, the result will not
include content related to the family, order, class, etc. to which the
organism is associated. This approach is contrast to other tools, such
as `taxonomyCleanr`, that build a complete taxonomic tree.

## See also

[`create_taxa_list`](create_taxa_list.md),
[`set_taxonomic_coverage`](set_taxonomic_coverage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Assumes taxa_df.csv is present in the working directory
  coverage <- create_taxonomicCoverage()
  str(coverage, max.level = 3)
} # }
```
