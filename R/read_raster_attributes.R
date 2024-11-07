#' @title Read raster attribute information from attributes metadata file
#'
#' @description The \code{read_raster_attributes} function reads a raster's
#' attribute details from a "entity name"_attrs.yaml or "entity name"_attrs.csv
#' file in the working directory - the details of which are incorporated into
#' the EML metadata for that entity. 
#'
#' @description The \code{read_raster_attributes} function reads a raster's
#' attribute details from a "entity name"_attrs.yaml or "entity name"_attrs.csv
#' file in the working directory - the details of which are incorporated into
#' the EML metadata for that entity. 
#'
#' @note Recent versions of the capeml package generate attribute and factor
#' metadata files in yaml format; the \code{read_raster_attributes} function
#' will look also for attributes files in csv format to accommodate older
#' projects.
#'
#' @note The \code{read_raster_attributes} function is intended primarily as a
#' helper to other functions in the capeml ecosystem so is not meant to be
#' called directly (but can be).
#'
#' @param entity_name
#' (character) The quoted name of the data entity.
#' @param entity_id
#' (character) Quoted identifier of the data object that is being described,
#' this will usually be the name or hash of the data table (or otherwise) of
#' which the attribute is associated.
#'
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom utils read.csv
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom tibble tibble add_row enframe
#' @importFrom dplyr pull select select_if
#' @importFrom EML set_attributes
#'
#' @return A entity of type EML attributes or list of attributes (for testing
#' and debugging)
#'
#' @export
#'
read_raster_attributes <- function(
  entity_name,
  entity_id = "data_entity"
  ) {

  # entity_name <- basename(tools::file_path_sans_ext(entity_name))

  # establish references to the data entity and entity name

  if (rlang::is_expression(entity_name)) {

    string_pointer <- rlang::get_expr(entity_name)
    # object_pointer <- get(x = entity_name, envir = globalenv())

  } else {

    string_pointer <- deparse(substitute(entity_name))
    # object_pointer <- entity_name

  }


  # attributes ----------------------------------------------------------------

  attrs <- read_attributes_file(
    string_pointer,
    entity_id
  )

  # # load attributes from yaml or csv (default to yaml)
  # if (file.exists(paste0(string_pointer, "_attrs.yaml"))) {
  #
  #   attrs <- yaml::yaml.load_file(paste0(string_pointer, "_attrs.yaml"))
  #   attrs <- yaml::yaml.load(attrs)
  #   attrs <- tibble::enframe(attrs) |>
  #     tidyr::unnest_wider(value) |>
  #     dplyr::select(-one_of("name"))
  #
  # } else if (!file.exists(paste0(string_pointer, "_attrs.yaml")) && file.exists(paste0(string_pointer, "_attrs.csv"))) {
  #
  #   attrs <- utils::read.csv(paste0(string_pointer, "_attrs.csv"))
  #
  # } else {
  #
  #   stop(paste0("attributes file: ", string_pointer, "_attrs.yaml ", "not found in ", getwd()))
  #
  # }
  #
  # # column classes to vector (req'd by set_attributes)
  # classes <- attrs |>
  #   dplyr::pull(columnClasses)
  #
  # # copy attributeDefinition to defintion as appropriate;
  # # remove col classes from attrs (req'd by set_attributes);
  # # remove empty columns (targets here are max and min values, which can throw
  # # an error for data without any numeric columns)
  # # empty strings to NA
  #
  # attrs[attrs == ""] <- NA
  #
  # # helper function to remove missing columns
  # not_all_na <- function(x) {
  #   !all(is.na(x))
  # }
  #
  # attrs <- attrs |>
  #   dplyr::mutate(
  #     id         = paste0(entity_id, "_", row.names(attrs)),
  #     definition = NA_character_,
  #     definition = dplyr::case_when(
  #       grepl("character", columnClasses) & ((is.na(definition) | definition == "")) ~ attributeDefinition,
  #       TRUE ~ definition
  #     )
  #     ) |>
  #   dplyr::select(-columnClasses) |>
  #   dplyr::select_if(not_all_na)


  # return --------------------------------------------------------------------

  attr_list <- EML::set_attributes(
    attributes    = attrs[["attrs"]],
    col_classes   = attrs[["classes"]]
  )

  attrs["columnClasses"] <- attrs[["classes"]]

  return(
    list(
      eml   = attr_list,
      table = attrs[["attrs"]]
    )
  )

}
