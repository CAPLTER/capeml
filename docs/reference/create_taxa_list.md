# Create a list of taxonomic classifications

This function generates a list of taxonomic classification records where
each record is a single-rank list corresponding to the value in the
`taxonRank` column of the input data frame. The output structure is
designed to match the EML (Ecological Metadata Language)
`classifications` format, with each element containing only the
specified rank, its value, and (if present) associated common names and
taxon identifiers.

## Usage

``` r
create_taxa_list(taxa_df)
```

## Arguments

- taxa_df:

  A data frame containing taxonomic information. Must include at least
  the columns `taxonRank`, `taxa_clean` (the value for the specified
  rank), `taxonID` (e.g., "ITIS:12345" or "GBIF:67890"), and optionally
  `vernacularName` (common names, possibly comma- or
  semicolon-separated).

## Value

A list of lists:

- `taxonRankName`: The rank name (from `taxonRank`).

- `taxonRankValue`: The value for that rank (from `taxa_clean`).

- `commonName`: A list of common names (if present in `vernacularName`);
  omitted if not present.

- `taxonId`: A list with `provider` and `taxonId`

## Details

Only the rank specified in `taxonRank` is included for each record. The
function is robust to missing or empty `vernacularName` and `taxonID`
fields. The output is suitable for use in EML metadata or other
workflows requiring a flat, single-rank taxonomic classification
structure.

## Examples

``` r
if (FALSE) { # \dontrun{
  taxa_df <- data.frame(
    taxonRank = c("family", "genus"),
    taxa_clean = c("Felidae", "Panthera"),
    taxonID = c("ITIS:183833", "ITIS:180544"),
    vernacularName = c("cats", "big cats")
  )
  taxon_trees <- create_taxa_list(taxa_df)
  str(taxon_trees[[1]], max.level = 2)
} # }
```
