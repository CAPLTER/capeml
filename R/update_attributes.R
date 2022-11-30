#' @title Update minimum and maximum values of numeric variables in existing
#' attribute metadata
#'
#' @description A frequent need with long-term, ongoing research is to update
#' existing data. A challenge to that is that we do not want to have to rebuild
#' from scratch the attribute metadata for a data entity at each update. In
#' terms of attribute metadata, definitions, units, etc. are relatively static
#' but what can change are the minimum and maximum values for numeric variables
#' as the observation record grows. We could ascertain the minimum and maximum
#' values for numeric variables then manually update existing attribute
#' metadata but this is tedious, error-prone, and can be time consuming when
#' dealing with many variables. The \code{update_attributes} function takes
#' care of this for us by reading the existing attribute metadata for a given
#' data entity and updating those metadata with the minimum and maximum values
#' for said data entity if they have changed in the context of a data refresh.
#'
#' @note An artifact of the updating process is that the \code{definition}
#' element is populated in the updated yaml file. The \code{definition} element
#' is required by the EML schema for attributes of type character. In a typical
#' workflow, the \code{definition} element is left blank in the metadata yaml
#' file and is automatically populated with a copy of the
#' \code{attributeDefinition} element at build time (e.g.,
#' \code{create_dataTable}). Because \code{update_attributes} is calling
#' \code{read_attributes}, which is what populates any empty \code{definition}s
#' for variables of type character, this is reflected by
#' \code{update_attributes} in the updated yaml. This does not have any bearing
#' on the resulting data entity EML, and we can still provide custom
#' \code{definition} metadata if desired.
#'
#' @note \code{update_attributes} will abort the update if an attribute is
#' detected in the data entity but for which there is not metadata in the
#' existing attributes file. This is an indication that the data structure or
#' content has changed sufficiently that a new, blank attributes metadata file
#' should be constructed. Conversely, if an attribute is detected in the
#' existing metadata that is not detected in the data entity, the update will
#' proceed but the attribute and corresponding metadata in the entity but not
#' in the existing metadata file will be stricken from the updated attribute
#' metadata file. In both cases, \code{update_attributes} will print to screen
#' the incongruent attributes.
#'
#'
#' @param entity_name
#' (character) The name of the data entity.
#' @param return_type
#' (character) Quoted designator indicating the value returned as either a
#' attributes template yaml file (return_type = "yaml", the default) or a list
#' of entity attributes (return_type = "attributes") constructed from the data
#' entity. The latter (i.e., return_type = "attributes") is really just for
#' testing the function.
#'
#' @importFrom dplyr inner_join mutate select bind_rows
#' @importFrom yaml write_yaml
#' @importFrom rlang is_expression get_expr
#' @importFrom purrr transpose
#'
#' @return An updated metadata template including refreshed minimum and maximum
#' vales for numeric variables for providing attribute metadata as a yaml file
#' with the file name of the R data object + "_attrs.yaml" in the working
#' directory.
#'
#' @examples
#' \dontrun{
#'
#'  # update attributes file for mycars data object
#'
#'  mycars <- head(mtcars)
#'
#'  capeml::update_attributes(entity_name = mycars)
#'
#' }
#'
#' @export
#'
update_attributes <- function(
  entity_name,
  return_type = "yaml"
  ) {

  # establish references to the data entity and entity name

  if (rlang::is_expression(entity_name)) {

    string_pointer <- rlang::get_expr(entity_name)
    object_pointer <- get(entity_name)

  } else {

    string_pointer <- deparse(substitute(entity_name))
    object_pointer <- entity_name

  }


  # read entity and yaml file data

  attrs_from_read  <- capeml::read_attributes(
    entity_name = string_pointer,
    return_type = "attributes"
  )

  attrs_from_write <- capeml::write_attributes(
    dfname      = object_pointer,
    return_type = "attributes"
  )

  attrs_from_write <- dplyr::bind_rows(attrs_from_write) |>
  dplyr::select(
    attributeName,
    new_min = minimum,
    new_max = maximum
  )


  # resolve mismatches between the current entity and existing metadata

  if (!identical(attrs_from_read[["attributeName"]], attrs_from_write[["attributeName"]])) { 

    not_in_read <- dplyr::anti_join(
      x  = attrs_from_write,
      y  = attrs_from_read,
      by = c("attributeName")
      ) |>
    dplyr::pull(attributeName)

    if (length(not_in_read) > 0) {

      not_in_read <- paste(not_in_read, collapse = " ")
      stop("aborted, these attributes not in the existing metadata: ", not_in_read)

    }

    not_in_write <- dplyr::anti_join(
      x  = attrs_from_read,
      y  = attrs_from_write,
      by = c("attributeName")
      ) |>
    dplyr::pull(attributeName)

    if (length(not_in_write) > 0) {

      not_in_write <- paste(not_in_write, collapse = " ")
      warning("attribute(s) in data entity but not metadata: ", not_in_write)

    }

  }


  # join and update

  # attrs_updated <- dplyr::inner_join(
  attrs_updated <- dplyr::right_join(
    x  = attrs_from_read,
    y  = attrs_from_write,
    by = c("attributeName")
    ) |>
  dplyr::mutate(
    minimum = new_min,
    maximum = new_max
    ) |>
  dplyr::select(
    -new_min,
    -new_max
  )


  # convert table back to a list

  attrs_updated <- purrr::transpose(
    .l     = attrs_updated,
    .names = attrs_updated[["attributeName"]]
  )

  attrs_updated <- lapply(attrs_updated, function(x) x[!is.na(x)])
  attrs_updated <- lapply(attrs_updated, function(x) x[x != ""])


  # conditional return

  if (grepl("yaml", return_type, ignore.case = TRUE)) {

    attrs_updated <- yaml::as.yaml(attrs_updated)

    file_name <- paste0(string_pointer, "_attrs.yaml")

    yaml::write_yaml(
      x    = attrs_updated,
      file = file_name
    )

    message(paste0("updated attribute yaml: ", file_name))

  } else if (grepl("attr", return_type, ignore.case = TRUE)) {

    return(attrs_updated)

  } else {

    stop("ambiguous return_type, should be 'yaml' or 'attributes'")

  }

}
