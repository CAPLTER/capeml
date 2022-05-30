#' @title Construct campel supporting files in a data package directory
#'
#' @description Create a directory structure for data package contents and
#' \code{capeml} files.
#'
#' @usage
#'     write_directory(
#'       scope,
#'       identifier,
#'       path
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
#'
#' @return
#'     A directory with the following structure and contents:
#'     \itemize{
#'         \item{\strong{name} Name supplied via \code{scope} and \code{identifier}}
#'         \itemize{
#'             \item{\strong{config.yaml} configuration file}
#'             \item{\code{data package name}.Rmd workflow template}
#'         }
#'     }
#'
#' @details Existing directories named with \code{data package name} at
#' \code{path} will not be overwritten.
#'
#' @examples
#' \dontrun{
#' # Template data package directory "edi.521"
#'
#' capeml::write_directory(
#'   scope      = "edi",
#'   identifier = 521,
#'   path       = '~/Desktop'
#' )
#'
#' }
#'
#' @export
#'
write_directory <- function(
  scope      = "knb-lter-cap",
  identifier,
  path       = "."
  ) {

  # do not proceed if a identifier is not provided

  if (missing("identifier")) {

    stop("write_directory missing package identifier (number)")

  }


  # package name

  package_name <- paste0(scope, ".", identifier)


  # stop if directory exists

  if (dir.exists(paste0(path, "/", package_name))) {
    stop(
      paste0(path, "/", package_name, " already exists")
    )
  }


# create parent dir ------------------------------------------------------------

  message(
    paste0(
      "generating ",
      path,
      "/",
      package_name
    )
  )

  dir.create(
    path = paste0(
      path,
      "/",
      package_name
    )
  )


# set directory to newly created -----------------------------------------------

  newParent <- paste0(
    path,
    "/",
    package_name
  )


# write config.yaml ------------------------------------------------------------

  write_config(
    scope      = scope,
    identifier = identifier,
    path       = newParent
  )


# generate capeml processing template ------------------------------------------

  write_template(
    scope      = scope,
    identifier = identifier,
    path       = newParent
  )


# end --------------------------------------------------------------------------

  message("completed generating directory, template, and config for ", package_name)

}
