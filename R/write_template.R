#' @title build a template workflow
#'
#' @description
#'     Write a workflow template Rmd file for the capeml workflow
#'
#' @usage
#'     write_template(
#'       packageScopeNumber,
#'       path,
#'       overwrite
#'     )
#'
#' @param packageScopeNumber
#'     (character) Quoted name of the package scope and number without the
#'     version number (e.g., "edi.521").
#' @param path
#'     (character) Path to where the data package directory will be created.
#'     Defaults to the current directory.
#' @param overwrite
#'  (logical) indicates to overwrite an existing file if one exists;
#'  default = FALSE
#'
#' @return
#'     A Rmarkdown (Rmd) file named with the project scope and number
#'
#' @details
#'     Existing files named with \code{packageScopeNumber}.Rmd at \code{path}
#'     will not be overwritten unless overwrite is made explicit.
#'
#' @examples
#' \dontrun{
#' # Template data package directory "edi.521"
#'
#' write_template(
#'   packageScopeNumber = "edi.521",
#'   path = '~/Desktop'
#' )
#'
#' write_template(
#'   packageScopeNumber = "edi.521"
#' )
#'
#' }
#'
#' @export
#'

write_template <- function(
  packageScopeNumber,
  path = ".",
  overwrite = FALSE) {

  # stop if file exists

  if (file.exists(paste0(path, "/", packageScopeNumber, ".Rmd")) & overwrite == FALSE) {
    stop(
      paste0(path, "/", packageScopeNumber, ".Rmd", " already exists - use overwrite")
    )
  }


  # generate capeml processing template ------------------------------------------

  file.copy(
    from = system.file(
      "/templates/workflow.Rmd",
      package = "capeml"
      ),
    to = paste0(
      path,
      "/",
      packageScopeNumber,
      ".Rmd"
    )
  )

  #   end ------------------------------------------------------------------------

  message(paste0("created ", packageScopeNumber, ".Rmd at ", path))

}
