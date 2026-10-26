# Locate an attribute or factor metadata template

`find_template` locates an entity's attribute or factor metadata
template (e.g., my_table_attrs.yaml) in the `metadata_path` directory or
the working directory.

## Usage

``` r
find_template(
  name,
  type = c("attrs", "factors"),
  metadata_path = NULL,
  also = NULL,
  extensions = c("yaml", "csv"),
  must_exist = FALSE
)
```

## Arguments

- name:

  (character) The quoted name of the data entity.

- type:

  (character) The template type: "attrs" or "factors".

- metadata_path:

  (character) Directory of metadata templates. Defaults to
  `metadata_path` in config.yaml, else the working directory.

- also:

  (character) Additional directories to search (e.g., the directory of a
  data file for templates co-located with the data).

- extensions:

  (character) Template file extensions to consider, in order of
  precedence.

- must_exist:

  (logical) Whether to raise an error if a template is not found.

## Value

The path to the template, or NULL if a template is not found and
`must_exist` is FALSE.

## Details

Within a directory, a yaml template takes precedence over a csv
template. An error is raised if a template for the entity is found in
more than one directory so that a stale copy is never used silently.
