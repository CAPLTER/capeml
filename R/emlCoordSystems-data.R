#' EML-compliant coordinate system names
#'
#' EML has a defined list of coordinate systems that are permissible to generate
#' valid EML. emlCoordSystems is a list (as a data frame) of those string
#' values. The get_emlProjection() function attempts to match the coordinate
#' system metadata inherent in a spatial data entity with the corresponding
#' EML-compliant name listed in emlCoordSystems.
#'
#' @docType data
#'
#' @format data frame
#'
#' @source \url{https://knb.ecoinformatics.org/#external//emlparser/docs/eml-2.1.1/./eml-spatialReference.html}
"emlCoordSystems"
