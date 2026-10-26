# qudt_table: reference table of QUDT units

`qudt_table` contains units-of-measure details outlined by the
Quantities, Units, Dimensions and dataTypes organization
\[QUDT.org\](https://qudt.org/). The table provides a resource for
documenting units of type QUDT in EML metadata.

## Usage

``` r
qudt_table
```

## Format

A dataframe; rows will vary as QUDT adds units and columns may vary if
the query is altered.

- unit:

  unit URI

- label:

  short description of the unit

- hasDimensionVector:

  dimension vector of unit

- conversionMultiplier:

  multiplier to SI

- lang:

  language format (US or British english)

- name:

  unit identifier

## Source

<https://raw.githubusercontent.com/qudt/qudt-public-repo/master/vocab/unit/VOCAB_QUDT-UNITS-ALL-v2.1.ttl>

## Note

The unit `description` currently is commented out in the query owing to
poorly formatted text and character issues. This is not needed in the
current `capeml` workflow.

Even when `description` is included in the query, be cautious of
filtering by datatype (e.g., Latex) as even though `description` is
included as an optional return the filter will omit entries without a
datatype.

`ucumCode` currently is commented out in the query as including this
variable results in many duplicate entries that vary only by the
`ucumCode` value. `ucumCode` is not part of the `capeml` workflow;
consider aggregating these values into an array if needed in the future.

Return is restricted to entries noted as being documented in english (US
or UK).
