#' @title Construct a template capeml workflow
#'
#' @description Write a workflow template qmd file for the capeml workflow
#'
#' @note While the function does provide a generalized template for much of the
#' capeml workflow, creating data entities (e.g., dataTable, otherEntity) are
#' not included. Further, the template is geared heavily to drawing on a
#' metadata [template](https://globalfutures.asu.edu/caplter/data/submit-data/)
#' that is part of the data submission process for the CAP LTER.
#'
#' @usage
#'     capeml::write_template(
#'       scope,
#'       identifier,
#'       path,
#'       overwrite
#'     )
#'
#' @param scope
#'  (character) Quoted name of the package scope (e.g., "edi"). The default is
#'  "knb-lter-cap".
#' @param identifier
#'  (integer) Data package identifier (number).
#' @param path
#'  (character) Path to where the config file will be written. Defaults to the
#'  current directory.
#' @param overwrite
#'  (logical) indicates to overwrite an existing file if one exists (default =
#'  FALSE).
#'
#' @return A quarto (qmd) file named with the project scope and identifier
#' (number).
#'
#' @details Existing files with the package name and .qmd extension at
#' \code{path} will not be overwritten unless overwrite is made explicit.
#'
#' @examples
#' \dontrun{
#' # Template data package directory "edi.521"
#'
#' capeml::write_template(
#'   scope      = "edi",
#'   identifier = 521,
#'   path       = '~/Desktop'
#' )
#'
#' }
#'
#' @export
#'
write_template <- function(
  scope      = "knb-lter-cap",
  identifier,
  path       = ".",
  overwrite  = FALSE
  ) {

  # do not proceed if a identifier is not provided

  if (missing("identifier")) {

    stop("write_directory missing package identifier (number)")

  }


  # package name

  package_name <- paste0(scope, ".", identifier)


  # stop if file exists

  if (file.exists(paste0(path, "/", package_name, ".qmd")) & overwrite == FALSE) {
    stop(
      paste0(path, "/", package_name, ".qmd", " already exists - use overwrite")
    )
  }


  # generate capeml processing template ------------------------------------------

  file.copy(
    from = system.file(
      "/templates/workflow.qmd",
      package = "capeml"
      ),
    to = paste0(
      path,
      "/",
      package_name,
      ".qmd"
    )
  )

  # end ------------------------------------------------------------------------

  message(paste0("created ", package_name, ".qmd at ", path))

}
