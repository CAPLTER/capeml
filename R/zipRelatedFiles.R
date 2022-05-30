#' @title zip related files (deprecated)
#'
#' @description Zip files in a common directory that share the same base name
#'  (i.e., file name sans path and extension). Zipped file includes all relevant
#'  files zipped into a single, compressed file named as: package number +
#'  basefile name + md5sum + extension (zip).
#'
#' @details zipRelatedFiles is a helper function designed primarily to assist
#'  the creation of EML spatial data objects. A spatial data entity often
#'  consists of more than one file (e.g., shapefiles; raster (*.img, *.xml))
#'  that are incorporated into a single zipped file for upload to PASTA. This
#'  function zips related files as determined by the same base file name (e.g.,
#'  CAP_1985.img and CAP_1985.img.aux.xml share the same base name and would be
#'  zipped). In keeping with the CAP LTER workflow for generating EML metadata,
#'  the resulting compressed file is named package number + basefile name +
#'  md5sum + extension (zip). Though intended primarily as a helper function,
#'  zipRelatedFiles can be run independently.
#'
#' @note zipRelatedFiles is specific to UNIX operating environments.
#'
#' @param pathToFile quoted name of the directory where the target file is
#'   located directory
#' @param targetFile quoted name of target data file to which other file names
#'   in the prescribed directory will be evalated directory
#'
#' @importFrom utils zip
#' @importFrom stringr str_extract
#' @importFrom tools md5sum
#' @importFrom yaml yaml.load_file
#'
#' @return the zipped file name for inclusion in the EML documentation - not
#'   relevant when run independently
#'
#' @examples
#' \dontrun{
#' zipRelatedFiles(
#'   pathToFile = "~/folder/",
#'   targetFile = "CAP_1985.img")
#' }
#'

zipRelatedFiles <- function(pathToFile, targetFile) {

  # deprecation ---------------------------------------------------------------

  .Deprecated(
    new = "none",
    package="capemlGIS",
    old = as.character(sys.call(sys.parent()))[1L]
  )

  stop()

  # check for requisite environment variables, function arguments, and that the target file exists in the
  # specified location

  # do not proceed if the path to where raster data reside is not provided
  if (!exists("pathToFile")) { stop("specify the path to directory with data") }

  # do not proceed if a target file is not provided
  if (!exists("targetFile")) { stop("specify the target file") }

  # do not proceed if the target file is not in the prescribed directory
  if(!file.exists(paste0(pathToFile, "/", basename(targetFile)))) {
    stop("target file is not in the prescribed directory")
  }

  # retrieve package number from config.yaml
  if (!file.exists("config.yaml")) {
    stop("config.yaml not found")
  }
  packageNum <- yaml::yaml.load_file("config.yaml")$packageNum

  # use full path - UNIX specific!
  pathToFile <- path.expand(pathToFile)


  # strip the raster name of its file extension
  targetFileBaseName <- str_extract(basename(targetFile), "^[^\\.]*")


  # we will need a temporary directory, ensure sure one with the same name does
  # not already exist
  temporaryDirectoryLocation <- paste0(pathToFile, "/temporaryDirectory")
  if (dir.exists(temporaryDirectoryLocation)) {
    unlink(temporaryDirectoryLocation, recursive = TRUE)
  }

  # create a temporary directory IF a directory with the base name of the target
  # file does not already exist as we will use this name
  namedDirectory <- paste0(pathToFile, "/", targetFileBaseName)

  if (dir.exists(namedDirectory)) {
    stop(paste0("a directory with the name '", namedDirectory, "' already exists"))
  } else {
    dir.create(temporaryDirectoryLocation)
  }


  # copy all files with the same name as passed into the same-named directory
  lapply(list.files(path = pathToFile, pattern = targetFileBaseName), function(x) {
    file.copy(paste0(pathToFile, "/", x), temporaryDirectoryLocation)
})


  # zip the directory with the target files, and rename with project id and hash

  # userDirectory <- getwd()
  # setwd(pathToFile)

  # rename the temporary directory to the base name of the target file
  file.rename(temporaryDirectoryLocation, namedDirectory)

  # zip the target directory
  system(paste0("zip -jX ", shQuote(namedDirectory, type = "sh"), " ", shQuote(namedDirectory, type = "sh"), "/*"))

  # mimic the name of the created zip entity
  created_zip_name <- paste0(targetFileBaseName, ".zip")

  # create a new zip name with project & hash
  new_zip_name <- paste0(packageNum, "_", targetFileBaseName, "_", tools::md5sum(paste0(pathToFile, "/", created_zip_name)), ".zip")

  # rename the created zip entity with the new name
  file.rename(paste0(pathToFile, "/", created_zip_name), paste0(pathToFile, "/", new_zip_name))

  # remove the unzipped directory
  unlink(namedDirectory, recursive = TRUE)


  # return the new zip file name for us in createEML functions
  return(new_zip_name)

}
