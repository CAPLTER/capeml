#' @title get the next version of a data package
#'
#' @description get_next_version will access the EDI API to identify and return
#' the next version of a data package. If the data package does not exist
#' (i.e., we are constructing a new package), get_next_version will return `1`
#' and display a corresponding note.
#'
#' @details get_next_version is a helper function designed to aid construction
#' of a package identifier (e.g., the version `5` of `knb-lter-cap.624.5`). As
#' such, the most common use case is that the function will be called
#' internally from capeml::createDataset but the function can be called
#' directly.
#'
#' @param provided_scope
#' character) scope of data package (defaults to CAP LTER: knb-lter-cap)
#' @param provided_identifier
#' (integer) identifier of data package
#'
#' @importFrom stringr str_extract
#' @importFrom EDIutils list_data_package_scopes list_data_package_revisions
#'
#' @return integer reflecting the next version of a data package
#'
#' @examples
#' \dontrun{
#' 
#' get_next_version(
#'   provided_scope      = "knb-lter-cap",
#'   provided_identifier = 624
#' )
#'
#' }
#' #'
#'
#' @export
#'
get_next_version <- function(
  provided_scope = "knb-lter-cap",
  provided_identifier
  ) {

  # ensure supplied scope features scope only (i.e., not package number or version)
  cleaned_scope <- stringr::str_extract(
    string  = provided_scope,
    pattern = "^[^\\.]*"
  )

  # confirm valid scope
  if (!cleaned_scope %in% c(EDIutils::list_data_package_scopes())) {

    stop(provided_scope, " is not a valid scope")

  }

  newest_version <- tryCatch({

    EDIutils::list_data_package_revisions(
      scope      = cleaned_scope,
      identifier = provided_identifier,
      filter     = "newest",
      env        = "production"
    )

  }, warning = function(cond) {

    message("warn: ", cond)

  }, error = function(cond) {

    message("dataset not found, assume new")
    return(0)

  })

  return(newest_version + 1)

}