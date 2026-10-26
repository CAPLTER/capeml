# write a template yaml file for adding personnel (people) metadata

Write a template yaml file (people.yaml) for adding pesonnel (people)
metadata

## Usage

``` r
write_people_template(path = ".", overwrite = FALSE)
```

## Arguments

- path:

  (character) Path to where the yaml file will be written. Defaults to
  the current directory.

- overwrite:

  (logical) Indicates to overwrite an existing file if one exists.

## Value

A people.yaml file in the prescribed directory for adding personnel
metadata; initially populated with inputs for a single \<creator\>,
\<metadataProvider\>, and \<associatedParty\>
