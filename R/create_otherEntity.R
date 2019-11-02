#' @title Create EML entity of type otherEntity
#'
#' @description create_otherEntity generates a EML entity of type otherEntity.
#'
#' @details A otherEntity entity is created from a single file (e.g.,
#'   desert_fertilization_sampling_sites.kml) or a directory. The resulting
#'   entity is renamed with the project id + base file name + md5sum + file
#'   extension. File extension is always .zip if the otherEntity is being
#'   created by zipping a directory.
#' @note create_otherEntity will look for a project id in the working
#'   environment; this parameter is not passed to the function and it must
#'   exist.
#' @note The target data file or directory can be located anywhere on a local
#'   computer but the renamed file with project id and hash will be written to
#'   the current working directory.
#'
#' @param targetFile The quoted name and path of the data file or directory.
#' @param description A description of the data entity.
#' @param baseURL The base path of the web-accessible location of the data file;
#'   the name of the resulting file will be passed to the base path to generate
#'   a web-resolvable file path.
#' @param overwrite If creating otherEntity by zipping a directory, this is a
#'   logical indicating whether to overwrite an already existing zip file that
#'   has the same name and location as the temporary zip object to be created.
#' @param projectNaming Logical indicating if the file or directory should be
#'   renamed per the style used by the CAP LTER (default) with the project id +
#'   base file name + md5sum + file extension. The passed file or directory name
#'   will be used if this parameter is set to FALSE.
#'
#' @import EML
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom tools md5sum file_ext
#' @importFrom stringr str_extract
#' @importFrom tools file_ext
#' @importFrom utils file_test
#'
#' @return EML entity of type otherEntity is returned. Additionally, the data
#'   file is renamed with the project id + base file name + md5sum + file
#'   extension.
#'
#' @examples
#' \dontrun{
#'
#'  # source file or dirctory can reside in or out of the working directory (but
#'  the output file will be written to the working directory)
#'
#'  desert_fertilization_sites <- create_otherEntity(
#'    targetFile = "~/Desktop/desert_fertilization_sampling_sites.kml",
#'    description = "approximate location of desert fertiliztion long-term study sites")
#'
#'  pass_codebook_2011 <- create_otherEntity(
#'    targetFile = "PASS-2011-Codebook-Feb2016rev.pdf",
#'    description = "PASS 2011 survey codebook")
#'
#'  pass_codebook_2011 <- create_otherEntity(
#'    targetFile = "~/Desktop/max_temperature",
#'    description = "rasters of max temperature years 2000-2016")
#'
#' }
#'
#' @export

create_otherEntity <- function(targetFile,
                               description,
                               baseURL = "https://data.gios.asu.edu/datasets/cap/",
                               overwrite = FALSE,
                               projectNaming = TRUE ) {

  # prerequisites -----------------------------------------------------------

  # do not proceed if the project id has not been identified in the working env
  if (projectNaming == TRUE & !exists('projectid')) { stop("missing project id") }

  # do not proceed if the target file or directly has not been provided
  if (missing('targetFile')) { stop("specify the name of the file or directory") }

  # default: targetFile is a file, not a directory
  isDirectory <- FALSE


  # zip if targetfile is a directory ----------------------------------------

  # create zip of directory if targetFile is in fact a directory
  if (file_test(op = "-d", x = targetFile)) {

    # flag that targetFile is a directory
    isDirectory <- TRUE

    # new object name: base name + zip extension
    zippedObject <- paste0(basename(targetFile), ".zip")

    # need full path to zip a directory
    targetFileFullPath <- path.expand(targetFile)

    # stop if zipping the directory will overwrite an existing object without
    # explicit overwrite - note that this is checking the existence of the
    # temporary object (e.g., dirname.zip), not the ultimate object (e.g.,
    # projectid_dirname_md5hash.zip)
    if (file.exists(zippedObject) && overwrite == FALSE) {
      stop("zipped object to be created with that name and location (e.g., targetfile.zip) already exists, change working directory or set overwrite to TRUE")
    }

    # zip the target directory
    system(paste0("zip -jXr ", shQuote(zippedObject, type = "sh"), " ", shQuote(targetFileFullPath, type = "sh")))

    targetFile <- zippedObject

  }


  # main function -----------------------------------------------------------

  # determine the file extension
  fileExtension <- tools::file_ext(targetFile)

  # set authentication (md5)
  fileAuthentication <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- tools::md5sum(targetFile)

  # set file size
  fileSize <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(targetFile))

  # set file format
  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat = EML::eml$externallyDefinedFormat(formatName = fileExtension)
  )

  targetFileBaseName <- basename(targetFile)
  directoryName <- dirname(targetFile)
  directoryNameFull <- sub("/$", "", path.expand(directoryName))
  pathToFile <- path.expand(targetFile)

  if (projectNaming == TRUE) {

    # if using project naming, add project-name specific elements to
    # spatialRaster entity

    # rename the existing file with new_file_name that features the project_id,
    # base name, md5sum hash, and extension
    new_file_name <- paste0(projectid, "_", str_extract(targetFileBaseName, "^[^\\.]*"), "_", md5sum(pathToFile), ".", fileExtension)
    file.copy(from = targetFile,
              to = paste0(directoryNameFull, "/", new_file_name))
    # file.rename(targetFile, new_file_name) # previous approach of renaming file instead of creating a copy with the new name

    # set distribution
    fileDistribution <- EML::eml$distribution(
      EML::eml$online(url = paste0(baseURL, new_file_name))
    )

    # build physical
    filePhysical <- EML::eml$physical(
      objectName = new_file_name,
      authentication = fileAuthentication,
      size = fileSize,
      dataFormat = fileDataFormat,
      distribution = fileDistribution
    )

    # create file object as otherEntity
    newOE <- EML::eml$otherEntity(
      entityName = new_file_name,
      entityDescription = description,
      physical = filePhysical,
      entityType = fileExtension,
      id = new_file_name
    )

    # close projectNaming == TRUE
  } else {

    # if not using not project naming, add source-name specific elements to
    # otherEntity object

    # rename the existing file with new_file_name that features the project_id,
    # base name, md5sum hash, and extension
    # targetFileBaseName <- basename(targetFile)
    # pathToFile <- path.expand(targetFile)
    # new_file_name <- paste0(projectid, "_", str_extract(targetFileBaseName, "^[^\\.]*"), "_", md5sum(pathToFile), ".", fileExtension)
    # file.copy(targetFile, new_file_name)
    # file.rename(targetFile, new_file_name) # previous approach of renaming file instead of creating a copy with the new name

    # set distribution
    fileDistribution <- EML::eml$distribution(
      EML::eml$online(url = paste0(baseURL, targetFileBaseName))
    )

    # build physical
    filePhysical <- EML::eml$physical(
      objectName = targetFileBaseName,
      authentication = fileAuthentication,
      size = fileSize,
      dataFormat = fileDataFormat,
      distribution = fileDistribution
    )

    # create file object as otherEntity
    newOE <- EML::eml$otherEntity(
      entityName = targetFileBaseName,
      entityDescription = description,
      physical = filePhysical,
      entityType = fileExtension,
      id = targetFileBaseName
    )

  }

  # if otherEntity created by zipping a directory, remove the temporary zip object (e.g., targetFile.zip)
  if (isDirectory == TRUE && file.exists(zippedObject)) {
    file.remove(zippedObject)
  }


  # return other entity object ----------------------------------------------

  return(newOE )


} # close create_otherEntity
