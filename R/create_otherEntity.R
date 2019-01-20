#' @title Create EML entity of type otherEntity
#'
#' @description create_otherEntity generates a EML entity of type otherEntity.
#'
#' @details A otherEntity entity is created from a single file (e.g.,
#'   desert_fertilization_sampling_sites.kml). The resulting entity is renamed
#'   with the project id + base file name + md5sum + file extension.
#' @note create_otherEntity will look for a project id in the working
#'   environment; this parameter is not passed to the function and it must
#'   exist.
#' @note The source data file can be located anywhere on a local computer but
#'   the renamed file with project id and hash will be written to the current
#'   working directory.
#'
#' @param file The quoted name and path of the data file.
#' @param description A description of the data.
#' @param baseURL The base path of the web-accessible location of the data file;
#'   the name of the resulting file will be passed to the base path to generate
#'   a web-resolvable file path.
#'
#' @import EML
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom tools md5sum file_ext
#' @importFrom stringr str_extract
#' @importFrom tools file_ext
#'
#' @return EML entity of type otherEntity is returned. Additionally, the data
#'   file is renamed with the project id + base file name + md5sum + file
#'   extension.
#'
#' @examples
#' \dontrun{
#'
#'  # source file can reside in or out of the working directory (but the output
#'  file will be written to the working directory)
#'
#'  desert_fertilization_sites <- create_otherEntity(
#'    targetFile = "~/Desktop/desert_fertilization_sampling_sites.kml",
#'    description = "approximate location of desert fertiliztion long-term study sites")
#'
#'  pass_codebook_2011 <- create_otherEntity(
#'    targetFile = "PASS-2011-Codebook-Feb2016rev.pdf",
#'    description = "PASS 2011 survey codebook")
#'
#' }
#'
#' @export


create_otherEntity <- function(targetFile, description, baseURL = "https://data.gios.asu.edu/datasets/cap/") {

  # determine the file extension
  fileExtension <- tools::file_ext(targetFile)

  # rename the existing file with new_file_name that features the project_id,
  # base name, md5sum hash, and extension
  targetFileBaseName <- basename(targetFile)
  pathToFile <- path.expand(targetFile)
  new_file_name <- paste0(projectid, "_", str_extract(targetFileBaseName, "^[^\\.]*"), "_", md5sum(pathToFile), ".", fileExtension)
  file.copy(targetFile, new_file_name)
  # file.rename(targetFile, new_file_name) # previous approach of renaming file instead of creating a copy with the new name

  # set authentication (md5)
  fileAuthentication <- eml$authentication(method = "MD5")
  fileAuthentication$authentication <- md5sum(new_file_name)

  # set file size
  fileSize <- eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(new_file_name))

  # set file format
  fileDataFormat <- eml$dataFormat(
    externallyDefinedFormat = eml$externallyDefinedFormat(formatName = fileExtension)
  )

  # set distribution
  fileDistribution <- eml$distribution(
    eml$online(url = paste0(baseURL, new_file_name))
  )

  # build physical
  filePhysical <- eml$physical(
    objectName = new_file_name,
    authentication = fileAuthentication,
    size = fileSize,
    dataFormat = fileDataFormat,
    distribution = fileDistribution
  )

  # create file object as otherEntity
  newfile <- eml$otherEntity(
    entityName = new_file_name,
    entityDescription = description,
    physical = filePhysical,
    entityType = fileExtension
  )

  return(newfile)

}
