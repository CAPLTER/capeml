# evaluate data package with EDI API

`get_package_evaluation` will use the generated eml.xml and uploaded
data to check the congruence of a data package.

## Usage

``` r
get_package_evaluation(identifier = NULL, scope = NULL, full_report = FALSE)
```

## Arguments

- identifier:

  (integer) Unquoted package identifier integer

- scope:

  (character) Quoted package scope (e.g., "knb-lter-cap", "edi")

- full_report:

  (logical) Logical indicating whether to return a summary (default) or
  detailed evaluation report.

## Value

summary or detailed evaluation report

## Note

This is a full evaluation of the data package, including checking
supplied metadata against provided data, as opposed to, for example,
just checking that the schema is valid. As such, the data files must be
available to EDI for proper evaluation.

`get_package_evaluation` will look for a config.yaml file to estalish
the package scope and identifier. Alternatively, these parameters can be
provided by the user but, in that case, both parameters must be
provided.

\`get_package_evaluation()\` uses the EDI API key stored in the
\`EDI_API_KEY\` environment variable.

If warns or errors are returned, we can view the full report to examine
the details of those problems. For example, write the report to file
with `xml2::write_xml(report, "/tmp/report.xml")`, and search for e.g.,
\`\<status\>warn\`.
