#' @title create_spatialRaster
#'
#' @description create_spatialRaster generates a EML entity of type
#'   spatialRaster
#'
#' @details a single data file (e.g., *.img) or a collection of related files
#'   (e.g., *.img and *.img.aux.xml) may be passed to create_spatialRaster. In
#'   the case of multiple file, all related files (as identified by a common
#'   base name) are aggregated into a single compressed (zipped) file. In all
#'   cases, the resulting entity is renamed with the project id + base file name
#'   + md5sum + file extension (zip in the case when multiple files are
#'   aggregated).
#'
#' @param rasterName the quoted name of the raster file (or base file in the
#'   case when the entity consists of multiple files)
#' @param rasterValueAttrs raster attribute and details
#' @param rasterValueFactors raster attribute details if attribute is of type
#'   enumerated domain
#' @param description quoted description of raster
#'
#' @import EML
#' @importFrom raster raster bandnr
#' @importFrom tools md5sum file_ext
#'
#' @return EML spatial data object is returned. Additionally, the spatial data
#'   file is renamed with the project id + base file name + md5sum + file
#'   extension (zip in the case when multiple files are aggregated).
#'
#' @export
#'
#' @examples
#' The current workflow includes hand-coding metadata given that there will
#' only ever be one attribute for a raster.
#'
#' Create attribute table manually, here calling the attribute 'value':
#' rasterValueAttrs <-
#'   data.frame(
#'     attributeName = c('value'),
#'     attributeLabel = c('categorical raster cell value'),
#'     attributeDefinition = c('categorical value...land use/land cover type'),
#'     definition = c('categorical value...land use/land cover type')
#'   )
#'
#' In this example, the raster value is of type enumerated domain so factor
#' details must be provided.
#'
#' Generate factor details, here details are pulled from a file where the
#' code and definition are detailed.
#' rasterValueFactors <-
#'   data.frame(
#'     attributeName = "value",
#'     code = rasterValuesMetadata$`Class ID`,
#'     definition = rasterValuesMetadata$`Class Name`
#'   )
#'
#' Create attribute list to pass to createSpatialRaster function
#' rasterAttributeList <- set_attributes(attributes = rasterValueAttrs,
#'                                       factors = rasterValueFactors,
#'                                       col_classes = c("factor"))
#'
#' \code {spatial entity <- create_spatialRaster('name of raster',
#'                                         rasterValueAttrs,
#'                                         rasterAttributeList,
#'                                         'Phoenix-area raster')}

create_spatialRaster <- function(rasterName, rasterValueAttrs, rasterValueFactors, description) {


  # do not proceed if the project id has not been identified in the working env
  if (!exists('projectid')) { stop("missing project id") }


  # read raster to access inherent file metadata
  rasterObject <- raster(rasterName)
  numBand <- new('numberOfBands',
                 bandnr(rasterObject))
  numRows <- new('rows',
                 nrow(rasterObject))
  numCols <- new('columns',
                 ncol(rasterObject))


  # call the get_emlProjection function to [attempt to] match the raster's
  # projection with the corresponding projection name that is pemissible in EML
  rasterProjection <- new('spatialReference',
                          horizCoordSysName = get_emlProjection(rasterObject))


  # parse file name and extension
  targetFileBaseName <- str_extract(basename(rasterName), "^[^\\.]*")
  targetFileExtension <- str_extract(rasterName, "[^\\.]+$")


  # if there are related files (e.g., supporting files) as determined by other
  # files with the same name as the raster (but with different file extentions),
  # then zip all files of the same name by calling the zipRelatedFiles()
  # function, and that will be our object with dataFormat = zip; the name of the
  # zipped file is returned and passed to objectName for further processing.
  # Else, process only the raster file with dataFormat = the file's extension.
  if (length(list.files(pattern = targetFileBaseName)) > 1) {
    objectName <- zipRelatedFiles(rasterName)
    objectFormat <- 'zip'
  } else {
    expandedName <- paste0(projectid, "_", targetFileBaseName, "_", md5sum(rasterName), ".", targetFileExtension)
    file.rename(rasterName, expandedName)
    objectName <- expandedName
    objectFormat <- targetFileExtension
  }


  # EML: physicl

  # create @physical
  physical <- set_physical(objectName)

  # add file size and unit to @physical
  file_size <- new("size",
                   deparse(file.size(objectName)),
                   unit = "byte")
  physical@size <- file_size

  # add authentication type (here md5) to @physical
  md5 <- new("authentication",
             md5sum(objectName),
             method = "MD5")
  physical@authentication <- c(md5)

  # add distribution to @physical
  online_url <- new("online",
                    url = paste0("https://data.gios.asu.edu/datasets/cap/", objectName))
  file_dist <- new("distribution",
                   online = online_url)
  physical@distribution <- c(file_dist)

  # add zip format to physical
  ext_format <- new("externallyDefinedFormat",
                    formatName = objectFormat)
  dat_format <- new("dataFormat",
                    externallyDefinedFormat = ext_format)
  physical@dataFormat <- dat_format


  # compile components for @attributeList of @dataTable
  # ignore factors if they are not relevant to this dataset;
  # workflow assumes that raster data that are not factors are numeric
  if(missing(rasterValueFactors)) {
    attr_list <- set_attributes(attributes = rasterValueAttrs,
                                col_classes = c("numeric"))
  } else {
    attr_list <- set_attributes(attributes = rasterValueAttrs,
                                factors = rasterValueFactors,
                                col_classes = c("factor"))
  }


  # create spatialRaster
  newSR <- new("spatialRaster",
               entityName = objectName,
               entityDescription = description,
               physical = physical,
               attributeList = attr_list,
               id = objectName,
               numberOfBands = numBand,
               rows = numRows,
               columns = numCols,
               spatialReference = rasterProjection
  )


  # return the xml object
  return(newSR)


} # close create_spatialRaster
