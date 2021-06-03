#' @title Read information from an attributes metadata file
#'
#' @description The read_attributes function reads an entity's attribute
#' details from a "entity name"_attrs.yaml or "entity name"_.csv file in the
#' working directory - the details of which are incorporated into the EML
#' metadata for that entity.
#'
#' @details The read_attributes function reads an entity's attribute details
#' from a "entity name"_attrs.yaml or "entity name"_.csv file in the working
#' directory - the details of which are incorporated into the EML metadata for
#' that entity.
#'
#' @note Recent versions of the capeml package generates attribute files in
#' yaml format; the read_attributes function will look also for attributes
#' files in csv format to accommodate older projects.
#'
#' @note The read_attributes function is intended primarily as a helper to
#' other functions in the capeml ecosystem so is not meant to be called
#' directly (but can be).
#'
#' @param entity_name
#' (character) The unquoted name of the data entity associated with attribute
#' metadata
#'
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom utils read.csv
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom tibble enframe
#' @importFrom dplyr pull select
#' @importFrom EML set_attributes
#'
#' @return A entity of type EML attributes
#'
#' @export
#'
read_attributes <- function(entity_name) {

  string_name <- deparse(substitute(entity_name))

  # load attributes from yaml or csv (default to yaml)
  if (file.exists(paste0(string_name, "_attrs.yaml"))) {

    attrs <- yaml::yaml.load_file(paste0(string_name, "_attrs.yaml"))
    attrs <- yaml::yaml.load(attrs)
    attrs <- tibble::enframe(attrs) %>%
      tidyr::unnest_wider(value) %>%
      dplyr::select(-one_of("name"))

  } else if (!file.exists(paste0(string_name, "_attrs.yaml")) && file.exists(paste0(string_name, "_attrs.csv"))) {

    attrs <- utils::read.csv(paste0(string_name, "_attrs.csv"))

  } else {

    stop(paste0("attributes file: ", string_name, "_attrs.yaml ", "not found in ", getwd()))

  }

  # column classes to vector (req'd by set_attributes)
  classes <- attrs %>%
    dplyr::pull(columnClasses)

  # copy attributeDefinition to defintion as appropriate; remove col classes
  # from attrs (req'd by set_attributes); remove empty columns (real targets
  # here are maximum and minimum, which can throw an error for data without any numeric
  # cols)

  # helper function to remove missing columns
  not_all_na <- function(x) {
    !all(is.na(x))
  }

  attrs <- attrs %>%
    mutate(
      definition = case_when(
        grepl("character", columnClasses) & ((is.na(definition) | definition == "")) ~ attributeDefinition,
        TRUE ~ definition
      )
      ) %>%
  dplyr::select(-columnClasses) %>%
  dplyr::select_if(not_all_na)

  attr_list <- EML::set_attributes(attributes = attrs, col_classes = classes)

  return(attr_list)

}
