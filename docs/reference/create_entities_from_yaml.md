# create EML and data entities from a data resources yaml file

create_entities_from_yaml takes a single list of configuration details
for a single data entity harvested from a data resources yaml file. The
appropriate data entity object (e.g., EML::dataTable and side effects if
relevant (e.g., creating the entity resource)) are generated from the
passed configuration details. This is essentially identical to calling,
for example, my_table \<- capeml::create_dataTable(dfname = my_table,
description = my_table_desc, ...) except that the arguments for
create_dataTable, create_otherEntity, etc. are articulated in a data
resources yaml file that is read by create_dataset. Passing
create_entities_from_yaml to a purrr workflow (within create_dataset)
enables constructing all relevant data entities from a single yaml file.

## Usage

``` r
create_entities_from_yaml(entity_configuration)
```

## Arguments

- entity_configuration:

  (list) data entity configuration details for a single data entity read
  from a data resources yaml file

## Value

a data entity added to a list of that type in the parent or global
environment

## Details

An optional `metadata_path` key for an entity identifies the directory
of that entity's attribute and factor templates, overriding
`metadata_path` in config.yaml. The `target_file_or_directory` of an
otherEntity is resolved against `data_path` in config.yaml if not found
in the working directory.
