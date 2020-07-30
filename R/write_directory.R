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
#' @param path
#'     (character) Path to where the data package directory will be created.
#'     Defaults to the current directory.
#' @param packageScopeNumber
#'     (character) Quoted name of the package scope and number without the
#'     version number (e.g., "edi.521").
#'
#' @return
#'     A directory with the following structure and contents:
#'     \itemize{
#'         \item{\strong{name} Name supplied via \code{packageScopeNumber}}
#'         \itemize{
#'             \item{\strong{config.yaml} configuration file}
#'         }
#'     }
#'
#' @details
#'     Existing directories named with \code{packageScopeNumber} at \code{path} will not
#'     be overwritten.
#'
#' @examples
#' \dontrun{
#' # Template data package directory "edi.521"
#'
#' write_directory(
#'   path = '~/Desktop',
#'   packageScopeNumber = "edi.521"
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

write_directory <- function(packageScopeNumber, path = ".") {

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

  #   value <- file.copy(
  #     from = system.file(
  #       "/templates/run_EMLassemblyline.R",
  #       package = "EMLassemblyline"
  #     ),
  #     to = paste0(
  #       path,
  #       "/",
  #       packageScopeNumber,
  #       "/run_EMLassemblyline_for_",
  #       packageScopeNumber,
  #       ".R"
  #     )
  #   )

  message("done")

}
