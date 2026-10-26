# Configure EDI API key authentication

\`edi_login()\` ensures that an EDI API key is available to the current
R process. The key is read from the \`EDI_API_KEY\` environment
variable.

## Usage

``` r
edi_login(force = FALSE)
```

## Arguments

- force:

  (logical) Register the API key with \`EDIutils::login()\` even when
  \`EDI_API_KEY\` is already set in the current R session.

## Value

Invisibly returns \`TRUE\` when API key authentication is configured.

## Details

\`capeml\` calls this helper automatically before package functions that
use \`EDIutils\`. Call it directly in interactive workflows when you
need to use \`EDIutils\` yourself, for example with
\`EDIutils::get_provenance_metadata()\`.
