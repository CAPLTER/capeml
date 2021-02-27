#' @title create data package directory
#'
#' @description
#'     Create a directory structure for data package contents and
#'     \code{capeml} files.
#'
#' @usage
#'     write_directory(
#'       path,
#'       packageScopeNumber
#'     )
#'
#' @param packageScopeNumber
#'     (character) Quoted name of the package scope and number without the
#'     version number (e.g., "edi.521").
#' @param path
#'     (character) Path to where the data package directory will be created.
#'     Defaults to the current directory.
#'
#' @return
#'     A directory with the following structure and contents:
#'     \itemize{
#'         \item{\strong{name} Name supplied via \code{packageScopeNumber}}
#'         \itemize{
#'             \item{\strong{config.yaml} configuration file}
#'             \item{\code{packageScopeNumber}.Rmd workflow template}
#'         }
#'     }
#'
#' @details
#'     Existing directories named with \code{packageScopeNumber} at \code{path}
#'     will not be overwritten.
#'
#' @examples
#' \dontrun{
#' # Template data package directory "edi.521"
#'
#' write_directory(
#'   packageScopeNumber = "edi.521",
#'   path = '~/Desktop'
#' )
#'
#' write_directory(
#'   packageScopeNumber = "edi.521"
#' )
#'
#' # View directory contents
#' dir("edi.521")
#'
#' # Clean up
#' unlink("edi.521", recursive = TRUE)
#'
#' }
#'
#' @export
#'

write_directory <- function(
  packageScopeNumber,
  path = ".") {

  # stop if directory exists

  if (dir.exists(paste0(path, "/", packageScopeNumber))) {
    stop(
      paste0(path, "/", packageScopeNumber, " already exists")
    )
  }

# create parent dir ------------------------------------------------------------

  message(
    paste0(
      "generating ",
      path,
      "/",
      packageScopeNumber
    )
  )

  dir.create(
    path = paste0(
      path,
      "/",
      packageScopeNumber
    )
  )


# write config.yaml ------------------------------------------------------------

  newParent <- paste0(
    path,
    "/",
    packageScopeNumber
  )

  write_config(
    packageScopeNumber = packageScopeNumber,
    path = newParent
  )


# generate capeml processing template ------------------------------------------

  write_template(
    packageScopeNumber = packageScopeNumber,
    path = newParent
  )

#   end ------------------------------------------------------------------------

  message("completed generating directory, template, and config")

}
