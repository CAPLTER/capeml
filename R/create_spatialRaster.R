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
#' @note create_spatialRaster currently does not accept an argument for url to a
#'   data file; the package defaults to a CAP LTER specific URL, and this must
#'   be changed manually in the resulting xml
#' @note create_spatialRaster relies on the helper functions zipRelatedFiles and
#'   get_emlProjection, which are specific to UNIX operating systems.
#'
#' @param pathToRaster The quoted path of the raster directory.
#' @param metadataFile The quoted path and name of the raster metadata file (the
#'   name of the raster data to be processed is included as a parameter in this
#'   metadata file).
#' @param categoricalMetadataFile Included if raster values are categorical. The
#'   quoted path and name of the raster metadata file that provides said
#'   details.
#'
#' @import EML
#' @import dplyr
#' @importFrom raster raster bandnr xres yres
#' @importFrom readr read_csv
#' @importFrom tools md5sum file_ext
#' @importFrom stringr str_extract
#'
#' @return EML spatial data object is returned. Additionally, the spatial data
#'   file is renamed with the project id + base file name + md5sum + file
#'   extension (zip in the case when multiple files are aggregated).
#'
#' @examples
#' \dontrun{
#' The workflow features harvesting metadata from template files. Most metadata
#' are documented in a raster-level metadata file (e.g., CAP1985_metadata.csv
#' below). If raster values are categorical, an addiitonal metadata file is
#' required to document the categories (e.g., CAP1985_factors.csv below)
#'
#'  spatial_entity <- create_spatialRaster(
#'    pathToRaster = "~/folder/",
#'    metadataFile = "~/folder/CAP1985_metadata.csv",
#'    categoricalMetadataFile = "~/folder/CAP1985_factors.csv")
#' }
#'
#' @export

create_spatialRaster <- function(pathToRaster, metadataFile, categoricalMetadataFile) {


  # check for required environmental parameters and arguments

    # do not proceed if the project id has not been identified in the working env
    if (!exists('projectid')) { stop("missing project id") }

    # do not proceed if the path to where raster data reside is not provided
    if (!exists('pathToRaster')) { stop("specify the path to directory with raster data") }

    # do not proceed if a metadata file is not provided
    if (!exists('metadataFile')) { stop("specify the raster metadata file") }


  # use full path
  pathToRaster <- path.expand(pathToRaster)

  # load metadata file
  rasterMetadata <- read_csv(metadataFile)

  # acces name of raster file
  rasterFileName <- rasterMetadata %>%
    filter(metadata_entity == 'rasterName') %>%
    select(metadata_value) %>%
    unlist(., use.names = FALSE)

  # do not proceed if the raster file is not in the prescribed directory
  if(!file.exists(paste0(pathToRaster, "/", basename(rasterFileName)))) { stop("raster file is not in the prescribed directory") }

  # identify raster location and file
  targetRaster <- paste0(pathToRaster, "/", rasterFileName)


  # load the raster file
  rasterObject <- raster(targetRaster)


  # read raster to access inherent file metadata
  numBand <- new('numberOfBands',
                 bandnr(rasterObject))
  numRows <- new('rows',
                 nrow(rasterObject))
  numCols <- new('columns',
                 ncol(rasterObject))
  cellsX <- new('cellSizeXDirection',
                xres(rasterObject))
  cellsY <- new('cellSizeYDirection',
                yres(rasterObject))


  # call the get_emlProjection function to [attempt to] match the raster's
  # projection with the corresponding projection name that is pemissible in EML
  coordSystem <- tryCatch({

    get_emlProjection(rasterObject)

  }, warning = function(warn) {

    print(paste("WARNING: projection not resolved"))
    return("METADATA_NOT_AVAILABLE")

  }, error = function(err) {

    print(paste("WARNING: projection not resolved"))
    return("METADATA_NOT_AVAILABLE")

  }) # close try catch

  rasterProjection <- new('spatialReference',
                          horizCoordSysName = coordSystem)


  # parse base name of file without extension
  targetFileBaseName <- str_extract(basename(rasterFileName), "^[^\\.]*")

  # if there are related files (e.g., supporting files) as determined by other
  # files with the same name as the raster (but with different file extentions),
  # then zip all files of the same name by calling the zipRelatedFiles function,
  # and that will be our object with dataFormat = zip; the name of the zipped
  # file is returned and passed to objectName for further processing. Else,
  # process only the raster file with dataFormat = the file's extension.
  if (length(list.files(path = pathToRaster, pattern = targetFileBaseName)) > 1) {
    objectName <- zipRelatedFiles(pathToRaster, rasterFileName)
    zipIsTrue <- TRUE
  } else {
    expandedName <- paste0(projectid, "_", targetFileBaseName, "_", md5sum(targetRaster), ".", file_ext(targetRaster))
    file.rename(targetRaster, paste0(pathToRaster, expandedName))
    objectName <- expandedName
  }

  # generate path to newly created object
  newObjectLocation <- paste0(pathToRaster, objectName)


  # EML: physical

  # create @physical
  physical <- set_physical(objectName)

  # add file size and unit to @physical
  file_size <- new("size",
                   deparse(file.size(newObjectLocation)),
                   unit = "byte")
  physical@size <- file_size

  # add authentication type (here md5) to @physical
  md5 <- new("authentication",
             md5sum(newObjectLocation),
             method = "MD5")
  physical@authentication <- c(md5)

  # add distribution to @physical
  # set onlineURL according to CAP's system
  onlineURL <- "https://data.gios.asu.edu/datasets/cap/"

  online_url <- new("online",
                    url = paste0(onlineURL, objectName))
  file_dist <- new("distribution",
                   online = online_url)
  physical@distribution <- c(file_dist)

  # add raster format or zip to physical
  if(isTRUE(zipIsTrue)) {
    ext_format <- new("externallyDefinedFormat",
                      formatName = 'zip')
    dat_format <- new("dataFormat",
                      externallyDefinedFormat = ext_format)
    physical@dataFormat <- dat_format
  } else {
    ext_format <- new("externallyDefinedFormat",
                      formatName = file_ext(targetRaster))
    dat_format <- new("dataFormat",
                      externallyDefinedFormat = ext_format)
    physical@dataFormat <- dat_format
  }


  # generate raster attribute table
  rasterValueAttrs <- data.frame(
    attributeName = c('value'),
    attributeLabel = c(rasterMetadata %>%
                         filter(metadata_entity == 'rasterValueLabel') %>%
                         select(metadata_value) %>%
                         unlist(., use.names = FALSE)),
    attributeDefinition = c(rasterMetadata %>%
                              filter(metadata_entity == 'rasterValueDescription') %>%
                              select(metadata_value) %>%
                              unlist(., use.names = FALSE)),
    definition = c(rasterMetadata %>%
                     filter(metadata_entity == 'rasterValueDescription') %>%
                     select(metadata_value) %>%
                     unlist(., use.names = FALSE))
  )


  # compile components for @attributeList of @dataTable
  # ignore factors if they are not relevant to this dataset
  if(missing(categoricalMetadataFile)) {

    # determine raster value data type when not categorical
    nonFactor = case_when(
      is.numeric(getValues(rasterObject)) == TRUE ~ "numeric",
      is.character(getValues(rasterObject)) == TRUE ~ "character"
    )

    attr_list <- set_attributes(attributes = rasterValueAttrs,
                                col_classes = c(nonFactor))

  } else {

    # import categorical metadata
    rasterValueCategories <- read_csv(categoricalMetadataFile)

    rasterValueFactors <- data.frame(
      attributeName = "value",
      code = rasterValueCategories$rasterValue,
      definition = rasterValueCategories$categoryName
    )

    attr_list <- set_attributes(attributes = rasterValueAttrs,
                                factors = rasterValueFactors,
                                col_classes = c("factor"))
  }


  # create spatialRaster
  newSR <- new("spatialRaster",
               entityName = objectName,
               entityDescription = rasterMetadata %>%
                 filter(metadata_entity == 'rasterDescription') %>%
                 select(metadata_value) %>%
                 unlist(., use.names = FALSE),
               physical = physical,
               attributeList = attr_list,
               spatialReference = rasterProjection,
               horizontalAccuracy = rasterMetadata %>%
                 filter(metadata_entity == 'horizontalAccuracy') %>%
                 select(metadata_value) %>%
                 unlist(., use.names = FALSE),
               verticalAccuracy = rasterMetadata %>%
                 filter(metadata_entity == 'verticalAccuracy') %>%
                 select(metadata_value) %>%
                 unlist(., use.names = FALSE),
               cellSizeXDirection = cellsX,
               cellSizeYDirection = cellsY,
               numberOfBands = numBand,
               rasterOrigin = rasterMetadata %>%
                 filter(metadata_entity == 'rasterOrigin') %>%
                 select(metadata_value) %>%
                 unlist(., use.names = FALSE),
               rows = numRows,
               columns = numCols,
               verticals = rasterMetadata %>%
                 filter(metadata_entity == 'verticals') %>%
                 select(metadata_value) %>%
                 unlist(., use.names = FALSE),
               cellGeometry = rasterMetadata %>%
                 filter(metadata_entity == 'cellGeometry') %>%
                 select(metadata_value) %>%
                 unlist(., use.names = FALSE),
               id = objectName
  )


  # return the xml object
  return(newSR)


} # close create_spatialRaster
