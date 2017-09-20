#' @title zipRelatedFiles
#'
#' @description Zip files in a common directory that share the same base name
#'   (i.e., file name sans path and extension). Zipped file includes all
#'   relevant files zipped into a single, compressed file named as: projectid +
#'   basefile name + md5sum + extension (zip).
#'
#' @details zipRelatedFiles is a helper function designed primarily to assist
#'   the creation of EML spatial data objects. A spatial data entity often
#'   consists of more than one file (e.g., shapefiles; raster (*.img, *.xml))
#'   that are incorporated into a single zipped file for upload to PASTA. This
#'   function zips related files as determined by the same base file name (e.g.,
#'   CAP_1985.img and CAP_1985.img.aux.xml share the same base name and would be
#'   zipped). In keeping with the CAP LTER workflow for generating EML metadata,
#'   the resulting compressed file is named projectid + basefile name + md5sum +
#'   extension (zip). Though intended primarily as a helper function,
#'   zipRelatedFiles can be run independently.
#'
#' @note The target file must be in the current working directory.
#' @note zipRelatedFiles is specific to UNIX operating environments.
#'
#' @param targetFile quoted name of target data file in the current working
#'   directory
#'
#' @importFrom utils zip
#' @importFrom stringr str_extract
#' @importFrom tools md5sum
#'
#' @return the zipped file name for inclusion in the EML documentation - not
#'   relevant when run independently
#'
#' @examples
#' \dontrun{ zipRelatedFiles('CAP_1985.img') }
#'
#' @export

zipRelatedFiles <- function(targetFile) {

  # do not proceed if the target file is not in the working directory
  if(!file.exists(paste0('./', basename(targetFile)))) { stop("target file is not in the working directory") }

  # strip the raster name of its file extension
  targetFileBaseName <- str_extract(basename(targetFile), "^[^\\.]*")

  # we will need a temporary directory, ensure sure one with the same name does
  # not already exist
  if (dir.exists('temporaryDirectory')) { unlink('temporaryDirectory', recursive = TRUE) }

  # create a temporary directory if a directory with the base name of the target
  # file does not already exist as we will use this name
  if (dir.exists(targetFileBaseName)) {
    stop(paste0("directory name '", targetFileBaseName, "' already exists"))
  } else {
    dir.create('temporaryDirectory')
  }

  # copy all files with the same name as passed into the same-named directory
  lapply(list.files(pattern = targetFileBaseName), function(x) {
    file.copy(x, 'temporaryDirectory')
  })

  # zip the directory with the target files, and rename with project id and hash
  file.rename('temporaryDirectory', targetFileBaseName) # rename the temporary directory to the base name of the target file
  zip(zipfile = targetFileBaseName,
      files = paste0(targetFileBaseName, '/')) # zip the target directory
  created_zip_name <- paste0(targetFileBaseName, '.zip') # mimic the name of the created zip entity
  new_zip_name <- paste0(projectid, "_", targetFileBaseName, "_", md5sum(created_zip_name), ".zip") # create a new zip name with project & hash
  file.rename(created_zip_name, new_zip_name) # rename the created zip entity with the new name
  unlink(targetFileBaseName, recursive = TRUE) # remove the unzipped directory

  # return the new zip file name for us in createEML functions
  return(new_zip_name)

}
