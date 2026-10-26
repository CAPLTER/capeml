# translate personnel metadata to eml

`get_people` harvests metadata pertaining to deataset personnel (people)
documented in a \`people.yaml\` file and translates those metadata to a
lists appropriate for inclusion in a EML \<dataset\> as
\`EML::eml\$creator\`, \`EML::eml\$metadataProvider\`, and
\`EML::eml\$associatedParty\` as relevant.

## Usage

``` r
get_people()
```

## Value

lists of data personnel appropriate for inclusion in a EML \<dataset\>
as \`EML::eml\$creator\`, \`EML::eml\$metadataProvider\`, and
\`EML::eml\$associatedParty\` as relevant
