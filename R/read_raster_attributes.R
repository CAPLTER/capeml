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
#' @note The \code{read_raster_attributes} function is intended primarily as a
#' helper to other functions in the capeml ecosystem so is not meant to be
#' called directly (but can be).
#'
#' @note The \code{read_raster_attributes} function is a helper function to the
#' capemlGIS package. This separate function for attributes associated with
#' rasters (as opposed to any other data type with the \code{read_attributes}
#' function) is to avoid the tricky challenge of reading the data object into
#' the R environment that is integral to \code{read_attributes}; in some cases
#' we do read the raster object into the environment but not always so we do
#' not want to force it as would be the case with \code{read_attributes}.
#'
#' @param entity_name
#' (character) The quoted name of the data entity.
#' @param entity_id
#' (character) Quoted identifier of the data object that is being described,
#' this will usually be the name or hash of the data table (or otherwise) of
#' which the attribute is associated.
#'
#' @importFrom EML set_attributes
#' @importFrom rlang get_expr is_expression
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

  # establish references to the data entity and entity name
  if (rlang::is_expression(entity_name)) {

    string_pointer <- rlang::get_expr(entity_name)

  } else {

    string_pointer <- deparse(substitute(entity_name))

  }


  # attributes ----------------------------------------------------------------

  attrs <- read_attributes_file(
    string_pointer,
    entity_id
  )


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
