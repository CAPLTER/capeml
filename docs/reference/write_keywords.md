# write a keywords template file

`write_keywords` creates a template as a csv file for supplying dataset
keywords. The template csv file is pre-populated with CAP LTER-specific
keywords attached to every data set, and inludes the relevant thesaurii
(LTER controlled vocabular, LTER core areas, Creator Defined Keyword
Set, CAPLTER Keyword Set List). The keywords.csv file has the
appropriate structure to be read into the create_keywordSet function to
include in EML metadata for a dataset.

## Usage

``` r
write_keywords(path = ".", overwrite = FALSE)
```

## Arguments

- path:

  (character) Path to where the file will be written. Defaults to the
  current directory.

- overwrite:

  (logical) Indicates to overwrite an existing file if one exists.

## Value

output is a partially populated csv file titled keywords.csv

## Examples

``` r
 # write_keywords()
 # write_keywords(overwrite = TRUE)
```
