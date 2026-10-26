# Locate a data file or directory

`find_data_file` resolves the location of a data file or directory that
may reside in the working directory or in any of the directories
identified by `data_path` in config.yaml.

## Usage

``` r
find_data_file(file, data_path = NULL)
```

## Arguments

- file:

  (character) The quoted name (and, optionally, partial path) of the
  data file or directory.

- data_path:

  (character) One or more directories to search. Defaults to `data_path`
  in config.yaml.

## Value

The path to the data file or directory.

## Details

The search proceeds from least to most expensive: (1) the path as given
(absolute or relative to the working directory); (2) the path relative
to each `data_path` directory; (3) a recursive search of each
`data_path` directory for a file or directory whose path ends in `file`.
The recursive search is a fallback only because listing large directory
trees can be slow, particularly on networked or parallel file systems.
An error is raised if the target is not found or if more than one
candidate is found.

## Examples

``` r
if (FALSE) { # \dontrun{

 algae <- readr::read_csv(capeml::find_data_file("algae.csv"))

} # }
```
