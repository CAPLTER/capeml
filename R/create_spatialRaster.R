#' @title create_spatialRaster
#'
#' @description create_spatialRaster generates a EML entity of type
#'   spatialRaster
#'
#' @details a spatialRaster entity is created from a single data file (e.g.,
#'   CAP_1985.img) or a collection of related files (e.g., CAP_1985.img,
#'   CAP_1985.img.aux.xml). In the case of multiple file, all related files as
#'   identified by a common base name (e.g., 'CAP_1985') are aggregated into a
#'   single compressed (zipped) file. In all cases, the resulting entity is
#'   renamed with the project id + base file name + md5sum + file extension (zip
#'   in the case when multiple files are aggregated).
#' @note create_spatialRaster will look for a project id in the working
#'   environment; this parameter is not passed to the function and it must
#'   exist.
#' @note create_spatialRaster currently requires an address to the file for
#'   access from a local data catalog; if this parameter is missing, the
#'   function defaults to the CAP LTER's file storage location.
#' @note create_spatialRaster relies on the helper functions zipRelatedFiles and
#'   get_emlProjection, which are specific to UNIX operating systems.
#'
#' @param rasterName the quoted name of the raster file
#' @param rasterValueAttrs raster attribute and details
#' @param rasterValueFactors raster attribute details if attribute is of type
#'   enumerated domain
#' @param description quoted description of raster
#' @param onlineURL quoted address of data file when accessed through a local
#'   data catalog
#'
#' @import EML
#' @importFrom raster raster bandnr
#' @importFrom tools md5sum file_ext
#'
#' @return EML spatial data object is returned. Additionally, the spatial data
#'   file is renamed with the project id + base file name + md5sum + file
#'   extension (zip in the case when multiple files are aggregated).
#'
#' @examples
#' \dontrun{
#' The current workflow includes hand-coding metadata given that there will
#' only ever be one attribute for a raster.
#'
#' Create attribute table manually, here calling the attribute 'value':
#' rasterValueAttribute <-
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
#'
#' spatial_entity <- create_spatialRaster(rasterName = 'CAP_1985.img',
#'                                        rasterValueAttrs = rasterValueAttribute,
#'                                        rasterValueFactors = rasterValueFactors,
#'                                        description = 'Phoenix-area raster',
#'                                        onlineURL = 'https://data.gios.asu.edu/datasets/cap/'
#'                                        )
#' }
#'
#' @export

create_spatialRaster <- function(rasterName, rasterValueAttrs, rasterValueFactors, description, onlineURL) {

  # do not proceed if the project id has not been identified in the working env
  if (!exists('projectid')) { stop("missing project id") }

  # do not proceed if the target file is not in the working directory
  if(!file.exists(paste0('./', basename(rasterName)))) { stop("raster file is not in the working directory") }


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

  # parse base name of file without extension
  targetFileBaseName <- str_extract(basename(rasterName), "^[^\\.]*")

  # if there are related files (e.g., supporting files) as determined by other
  # files with the same name as the raster (but with different file extentions),
  # then zip all files of the same name by calling the zipRelatedFiles function,
  # and that will be our object with dataFormat = zip; the name of the zipped
  # file is returned and passed to objectName for further processing. Else,
  # process only the raster file with dataFormat = the file's extension.
  if (length(list.files(pattern = targetFileBaseName)) > 1) {
    objectName <- zipRelatedFiles(rasterName)
    objectFormat <- 'zip'
  } else {
    expandedName <- paste0(projectid, "_", targetFileBaseName, "_", md5sum(rasterName), ".", file_ext(rasterName))
    file.rename(rasterName, expandedName)
    objectName <- expandedName
  }


  # EML: physical

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
  # set onlineURL according to CAP's system if that parameter is missing
  if(missing(onlineURL)) { onlineURL <- "https://data.gios.asu.edu/datasets/cap/" }

  online_url <- new("online",
                    url = paste0(onlineURL, objectName))
  file_dist <- new("distribution",
                   online = online_url)
  physical@distribution <- c(file_dist)

  # add zip format to physical
  ext_format <- new("externallyDefinedFormat",
                    formatName = file_ext(rasterName))
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
