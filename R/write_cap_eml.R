#' @title write eml to file
#'
#' @description
#'     A wrapper around EML::write_eml that automates file naming from projet
#'     configuration
#'
#' @usage
#'     write_cap_eml(
#'       emlEntity,
#'       filePathName
#'     )
#'
#' @param emlEntity
#'     (unquoted character) Unquoted name of the eml entity (from
#'     `create_eml`) to be written to file. Defaults to `eml`.
#' @param filePathName
#'     (character) Path and file name of file to be written. Defaults to the
#'     package identifier and current directory.
#'
#' @return
#'     Writes eml object as specified name and to specified location in xml
#'     format.
#'
#' @details
#'     Any existing xml files named with \code{packageScopeNumber} at
#'     \code{path} will be overwritten.
#'
#' @examples
#' \dontrun{
#'
#' write_cap_eml(
#'   emlEntity = myeml,
#'   filePathName = "~/Desktop/myeml.xml"
#' )
#'
#' write_cap_eml()
#'
#' }
#'
#' @export
#'

write_cap_eml <- function(emlEntity = eml, filePathName) {

  if (missing(filePathName)) {

    # retrieve dataset details from config.yaml

    configurations <- read_package_configuration()


    # package version

    this_version <- capeml::get_next_version(
      provided_scope      = configurations$scope,
      provided_identifier = configurations$identifier
    )


    # package name (scope + identifier +  version)

    package_name <- paste(
      configurations$scope,
      configurations$identifier,
      this_version,
      sep = "."
    )


    filePathName <- paste0(package_name, ".xml")

  }

  # write the eml to file

  EML::write_eml(emlEntity, filePathName)


  # end ------------------------------------------------------------------------

  message("done")

}
