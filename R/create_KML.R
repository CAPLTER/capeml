#' @title create_KML
#'
#' @description create_KML generates a EML entity of type otherEntity with a kml
#'   dataType
#'
#' @details a otherEntity entity is created from a single kml file (e.g.,
#'   desert_fertilization_sampling_sites.kml). The resulting entity is renamed
#'   with the project id + base file name + md5sum + file extension (kml).
#' @note create_KML will look for a project id in the working environment; this
#'   parameter is not passed to the function and it must exist.
#' @note create_KML currently does not accept an argument for url to a data
#'   file; the package defaults to a CAP LTER specific URL, and this must be
#'   changed manually in the resulting xml
#' @note the source kml file can be located anywhere on a local computer but the
#'   renamed file with project id and hash will be written to the current
#'   working directory
#'
#' @param kmlFile The quoted name and path of the kml file.
#' @param description A description of the data.
#'
#' @import EML
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom tools md5sum file_ext
#' @importFrom stringr str_extract
#'
#' @return EML entity of type otherEntity with a kml dataType is returned.
#'   Additionally, the spatial data file is renamed with the project id + base
#'   file name + md5sum + file extension (kml).
#'
#' @examples
#' \dontrun{
#'
#'  # source file can reside in or out of the working directory (but the output
#'  file will be written to the working directory)
#'
#'  desert_fertilization_sites <- create_KML(
#'    kmlFile = "~/Desktop/desert_fertilization_sampling_sites.kml",
#'    description = "approximate location of desert fertiliztion long-term study sites")
#'
#'  desert_fertilization_sites <- create_KML(
#'    kmlFile = "desert_fertilization_sampling_sites.kml",
#'    description = "approximate location of desert fertiliztion long-term study sites")
#'
#' }
#'
#' @export


create_KML <- function(kmlFile, description) {

  # rename the existing kml file with kml_new_name that features the project_id,
  # base name, md5sum hash, and kml extension

  kmlFileBaseName <- basename(kmlFile)
  pathToFile <- path.expand(kmlFile)
  new_kml_name <- paste0(projectid, "_", str_extract(kmlFileBaseName, "^[^\\.]*"), "_", md5sum(pathToFile), ".kml")
  file.copy(kmlFile, new_kml_name)
  # file.rename(kmlFile, new_kml_name)

  # NEW

  # set authentication (md5)
  kmlAuthentication <- eml$authentication(method = "MD5")
  kmlAuthentication$authentication <- md5sum(new_kml_name)

  # set file size
  kmlSize <- eml$size(unit = "byte")
  kmlSize$size <- deparse(file.size(new_kml_name))

  # set file format to kml
  kmlDataFormat <- eml$dataFormat(
    externallyDefinedFormat = eml$externallyDefinedFormat(formatName = "kml")
  )

  # set distribution
  kmlDistribution <- eml$distribution(
    eml$online(url = paste0("https://data.gios.asu.edu/datasets/cap/", new_kml_name))
  )

  # build physical
  kmlPhysical <- eml$physical(
    objectName = new_kml_name,
    authentication = kmlAuthentication,
    size = kmlSize,
    dataFormat = kmlDataFormat,
    distribution = kmlDistribution
  )

  # create kml object as otherEntity
  newKML <- eml$otherEntity(
    entityName = new_kml_name,
    entityDescription = description,
    physical = kmlPhysical,
    entityType = "kml"
  )

  return(newKML)

}
