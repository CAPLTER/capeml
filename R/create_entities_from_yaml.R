#' @title create EML and data entities from a data resources yaml file
#'
#' @description create_entities_from_yaml takes a single list of configuration
#' details for a single data entity harvested from a data resources yaml file.
#' The appropriate data entity object (e.g., EML::dataTable and side effects if
#' relevant (e.g., creating the entity resource)) are generated from the passed
#' configuration details. This is essentially identical to calling, for
#' example, my_table <- capeml::create_dataTable(dfname = my_table, description
#' = my_table_desc, ...) except that the arguments for create_dataTable,
#' create_otherEntity, etc. are articulated in a data resources yaml file that
#' is read by create_dataset. Passing create_entities_from_yaml to a purrr
#' workflow (within create_dataset) enables constructing all relevant data
#' entities from a single yaml file.
#'
#' @param entity_configuration
#' (list) data entity configuration details for a single data entity read from
#' a data resources yaml file
#'
#' @importFrom tools file_path_sans_ext
#'
#' @return a data entity added to a list of that type in the parent or global
#' environment
#'
#' @export
#'
create_entities_from_yaml <- function(entity_configuration) {

  # build table EML

  if (entity_configuration$type == "table") {

    # eml_table_entity <- capeml::create_dataTable(
    eml_table_entity <- create_dataTable(
      dfname                 = entity_configuration$dfname,
      description            = entity_configuration$description,
      dateRangeField         = entity_configuration$dateRangeField,
      overwrite              = entity_configuration$overwrite,
      projectNaming          = entity_configuration$projectNaming,
      missingValueCode       = entity_configuration$missingValueCode,
      additional_information = entity_configuration$additional_information
    )

    return(
      list(
        type   = "table",
        entity = eml_table_entity
      )
    )

  }


  # build otherEntity EML

  if (entity_configuration$type == "other") {

    eml_other_entity <- capeml::create_otherEntity(
      target_file_or_directory = entity_configuration$target_file_or_directory,
      description              = entity_configuration$description,
      overwrite                = entity_configuration$overwrite,
      project_naming           = entity_configuration$projectNaming,
      additional_information   = entity_configuration$additional_information
    )

    entity_id <- basename(entity_configuration$target_file_or_directory)
    entity_id <- tools::file_path_sans_ext(entity_id)

    return(
      list(
        type   = "other",
        entity = eml_other_entity
      )
    )

  }

}
