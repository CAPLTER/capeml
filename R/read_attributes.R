#' @title Read entity attribute information from attributes and factors
#' metadata files, and document missing value codes
#'
#' @description The \code{read_attributes} function reads an entity's attribute
#' details from a "entity name"_attrs.yaml or "entity name"_attrs.csv file in
#' the working directory - the details of which are incorporated into the EML
#' metadata for that entity. If present in the working directory, factor
#' metadata in a "entity name"_factors.yaml or "entity name"_factors.csv are
#' read and incorporated into attribute metadata. NA, NaN, and user-provided
#' missing value codes are documented for each variable if they exist.
#'
#' @details The \code{read_attributes} function reads an entity's attribute
#' details from a "entity name"_attrs.yaml or "entity name"_attrs.csv file in
#' the working directory - the details of which are incorporated into the EML
#' metadata for that entity. If present in the working directory, factor
#' metadata in a "entity name"_factors.yaml or "entity name"_factors.csv are
#' read and incorporated into attribute metadata. NA, NaN, and user-provided
#' missing value codes are documented for each variable if they exist.
#'
#' @note The \code{read_attributes} function is specific to all data types
#' _except_ rasters. A sister function \code{read_raster_attributes} addresses
#' similar functionality for raster data.

#' @note Recent versions of the capeml package generate attribute and factor
#' metadata files in yaml format; the \code{read_attributes} function will look
#' also for attributes files in csv format to accommodate older projects.
#'
#' @note The \code{read_attributes} function is intended primarily as a helper
#' to other functions in the capeml ecosystem so is not meant to be called
#' directly (but can be).
#'
#' @note Missing value codes are not documented for geometry columns of spatial
#' (sf) objects.
#'
#' @param entity_name
#' (character) The quoted name of the data entity.
#' @param missing_value_code (optional)
#' (character) \code{read_attributes} will automatically document the presence
#' of NA and NaN entries as missing values in the EML output. The user has the
#' ability to identify an additional indicator of missing values (e.g.,
#' "-9999", "missing") if present.
#' @param entity_id
#' (character) Quoted identifier of the data object that is being described,
#' this will usually be the name or hash of the data table (or otherwise) of
#' which the attribute is associated.
#'
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom utils read.csv
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom tibble tibble enframe
#' @importFrom dplyr select 
#' @importFrom EML set_attributes
#' @importFrom purrr map_df
#' @importFrom sf st_drop_geometry
#' @importFrom rlang get_expr is_expression
#'
#' @return A entity of type EML attributes or list of attributes (for testing
#' and debugging)
#'
#' @export
#'
read_attributes <- function(
  entity_name,
  missing_value_code = NULL,
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

  attrs <- read_attributes_file(
    string_pointer,
    entity_id
  )

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

  attr_list <- EML::set_attributes(
    attributes    = attrs[["attrs"]],
    factors       = fcts,
    col_classes   = attrs[["classes"]],
    missingValues = mvframe
  )

  attrs["columnClasses"] <- attrs[["classes"]]

  return(
    list(
      eml   = attr_list,
      table = attrs[["attrs"]]
    )
  )

}
