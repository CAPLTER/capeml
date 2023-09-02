#' @title Read information from attributes and factors metadata files, and
#' document missing value codes
#'
#' @description The read_attributes function reads an entity's attribute
#' details from a "entity name"_attrs.yaml or "entity name"_attrs.csv file in
#' the working directory - the details of which are incorporated into the EML
#' metadata for that entity. If present in the working directory, factor
#' metadata in a "entity name"_factors.yaml or "entity name"_factors.csv are
#' read and incorporated into attribute metadata. NA, NaN, and user-provided
#' missing value codes are documented for each variable if they exist.
#'
#' @details The read_attributes function reads an entity's attribute
#' details from a "entity name"_attrs.yaml or "entity name"_attrs.csv file in
#' the working directory - the details of which are incorporated into the EML
#' metadata for that entity. If present in the working directory, factor
#' metadata in a "entity name"_factors.yaml or "entity name"_factors.csv are
#' read and incorporated into attribute metadata. NA, NaN, and user-provided
#' missing value codes are documented for each variable if they exist.
#'
#' @note Recent versions of the capeml package generate attribute and factor
#' metadata files in yaml format; the read_attributes function will look also
#' for attributes files in csv format to accommodate older projects.
#'
#' @note The read_attributes function is intended primarily as a helper to
#' other functions in the capeml ecosystem so is not meant to be called
#' directly (but can be).
#'
#' @note Missing value codes are not documented for geometry columns of spatial
#' (sf) objects.
#'
#' @param entity_name
#' (character) The quoted name of the data entity.
#' @param missing_value_code (optional)
#' (character) read_attributes will automatically document the presence of NA
#' and NaN entries as missing values in the EML output. The user has the
#' ability to identify an additional indicator of missing values (e.g.,
#' "-9999", "missing") if present.
#' @param return_type
#' (character) Quoted designator indicating the value returned as either an EML
#' attributes entity (return_type = "eml", the default) or a dataframe of
#' entity attributes and column classes (return_type = "attributes") read from
#' the attributes file, the latter primarily as a helper feature for updating
#' an existing attributes file. 
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
#' @importFrom purrr map_df
#' @importFrom sf st_drop_geometry
#'
#' @return A entity of type EML attributes or list of attributes (for testing
#' and debugging)
#'
#' @export
#'
read_attributes <- function(
  entity_name,
  missing_value_code = NULL,
  return_type        = "eml",
  entity_id          = "data_entity"
  ) {

  # establish references to the data entity and entity name

  if (rlang::is_expression(entity_name)) {

    string_pointer <- rlang::get_expr(entity_name)
    object_pointer <- get(x = entity_name, envir = globalenv())

  } else {

    string_pointer <- deparse(substitute(entity_name))
    object_pointer <- entity_name

  }


  # attributes ----------------------------------------------------------------

  # load attributes from yaml or csv (default to yaml)
  if (file.exists(paste0(string_pointer, "_attrs.yaml"))) {

    attrs <- yaml::yaml.load_file(paste0(string_pointer, "_attrs.yaml"))
    attrs <- yaml::yaml.load(attrs)
    attrs <- tibble::enframe(attrs) |>
      tidyr::unnest_wider(value) |>
      dplyr::select(-one_of("name"))

  } else if (!file.exists(paste0(string_pointer, "_attrs.yaml")) && file.exists(paste0(string_pointer, "_attrs.csv"))) {

    attrs <- utils::read.csv(paste0(string_pointer, "_attrs.csv"))

  } else {

    stop(paste0("attributes file: ", string_pointer, "_attrs.yaml ", "not found in ", getwd()))

  }

  # column classes to vector (req'd by set_attributes)
  classes <- attrs |>
    dplyr::pull(columnClasses)

  # copy attributeDefinition to defintion as appropriate;
  # remove col classes from attrs (req'd by set_attributes);
  # remove empty columns (targets here are max and min values, which can throw
  # an error for data without any numeric columns)
  # empty strings to NA

  attrs[attrs == ""] <- NA

  # helper function to remove missing columns
  not_all_na <- function(x) {
    !all(is.na(x))
  }

  attrs <- attrs |>
    dplyr::mutate(
      id         = paste0(entity_id, "_", row.names(attrs)),
      definition = NA_character_,
      definition = dplyr::case_when(
        grepl("character", columnClasses) & ((is.na(definition) | definition == "")) ~ attributeDefinition,
        TRUE ~ definition
      )
      ) |>
    dplyr::select(-columnClasses) |>
    dplyr::select_if(not_all_na)


  # factors -------------------------------------------------------------------

  fcts <- NULL

  # load factor metadata from yaml or csv (default to yaml)

  if (file.exists(paste0(string_pointer, "_factors.yaml"))) {

    fcts <- yaml.load_file(paste0(string_pointer, "_factors.yaml")) |>
      yaml::yaml.load() |>
      tibble::enframe() |>
      tidyr::unnest_wider(value) |>
      tidyr::unnest_wider(attribute) |>
      tidyr::unnest_longer(levels) |>
      tidyr::unnest_wider(levels) |>
      dplyr::select(-one_of("name"))

  } else if (file.exists(paste0(string_pointer, "_factors.csv"))) {

    fcts <- utils::read.csv(paste0(string_pointer, "_factors.csv"))

  }


  # missing value coding ------------------------------------------------------

  # drop geometry columns from consideration if simple features
  if (class(object_pointer)[[1]] == "sf") {

    object_pointer <- object_pointer |>
      sf::st_drop_geometry()

  }

  missing_value_frame <- tibble::tibble(
    attributeName = as.character(),
    code          = as.character(),
    definition    = as.character()
  )

  mvframe <- purrr::map_df(
    .x         = colnames(object_pointer),
    .f         = capeml::write_missing_values,
    storage    = missing_value_frame,
    dataObject = object_pointer,
    MVC        = missing_value_code
  )

  if (nrow(mvframe) == 0) {

    mvframe <- NULL

  }


  # return --------------------------------------------------------------------

  if (grepl("eml", return_type, ignore.case = TRUE)) {

    attr_list <- EML::set_attributes(
      attributes    = attrs,
      factors       = fcts,
      col_classes   = classes,
      missingValues = mvframe
    )

    return(attr_list)

  } else if (grepl("attr", return_type, ignore.case = TRUE)) {

    attrs["columnClasses"] <- classes

    return(attrs)

  } else {

    stop("ambiguous return_type, should be 'eml' or 'attributes'")

  }

}
