#' @title create a EML entity of type EML
#'
#' @description \code{create_eml} generates a EML entity of type eml from
#' objects and metadata compiled from capeml objects in the working directory
#'
#' @details A eml entity is created from objects defined in the user's R
#' environment and configuration files. 
#'
#' @note \code{create_eml} will look for most inputs used to construct a eml
#' entity, such as access and dataset, in the working environment; these
#' parameters are not passed directly to the function and must exist in the
#' working environment.
#'
#' @note Some parameters, such access, are loaded in the backgroud when the
#' package is loaded and not called directly by the user.
#'
#' @import EML
#'
#' @return EML entity of type EML is returned.
#'
#' @export
#'
create_eml <- function() {

  # confirm required components exist in R environment

  if (!exists("lterAccess")) { stop("missing access") }
  if (!exists("dataset")) { stop("missing dataset") }


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

  # construct eml

  eml <- EML::eml$eml(
    access    = lterAccess,
    dataset   = dataset,
    packageId = package_name,
    system    = "knb",
    scope     = "system"
  )


  # custom units and unit annotations

  if (file.exists("custom_units.yaml")) {

    this_unit_list <- capeml::get_custom_units()
    eml$additionalMetadata <- this_unit_list

  }


  # annotations

  if (file.exists("annotations.yaml")) {

    this_annotation_list <- capeml::get_annotations()
    eml$annotations <- this_annotation_list

  }


  return(eml)

}
