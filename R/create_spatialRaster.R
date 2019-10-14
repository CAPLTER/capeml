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
#' @param rasterFile Quoted full path to raster file.
#' @param description Description of the raster.
#' @param epsgProjection EPSG numeric code of raster's coordinate reference system
#' @param emlProjection EML-compliant refence to raster's coordinate reference system
#' @param rasterValueDescription Description of raster values
#' @param rasterValueUnits Raster value units
#' @param zipFiles Logical indicating whether spatial raster entity should be
#'   constructed from a single raster file (FALSE, default) or entire directory (TRUE)
#' @param baseURL (optional) The base path of the web-accessible location of the
#'   data file; the name of the resulting file will be passed to the base path
#'   to generate a web-resolvable file path.
#'
#' @import EML
#' @import dplyr
#' @importFrom raster
#' @importFrom readr read_csv
#' @importFrom tools md5sum file_ext
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

create_spatialRaster <- function(rasterFile,
                                 description,
                                 epsgProjection,
                                 emlProjection,
                                 rasterValueDescription,
                                 rasterValueUnits,
                                 zipFiles = FALSE,
                                 baseURL = "https://data.gios.asu.edu/datasets/cap/") {


  # set options -------------------------------------------------------------

  options(scipen = 999)


  # required parameters -----------------------------------------------------

  # do not proceed if the project id has not been identified in the working env
  if (!exists('projectid')) { stop("missing project id") }

  # do not proceed if a description is not provided
  if (missing('description')) { stop("please provide a description for this raster") }

  # do not proceed if a description of the rater values is not provided
  if (missing('rasterValueDescription')) { stop("please provide a desription of the raster cell values") }

  # do not proceed if a epsg of EML-compliant projection is not provided
  if (missing('epsgProjection') & missing('emlProjection')) {

    stop("please provide a EPSG -or- EML-compliant projection for this raster")

  }


  # load raster -------------------------------------------------------------

  rasterObject <- raster(rasterFile)


  # establish raster file parent directory ----------------------------------

  directoryName <- dirname(rasterFile)


  # build attribute table ---------------------------------------------------

  rasterFactorsFileName <- paste0(directoryName, "/", basename(file_path_sans_ext(rasterFile)), "_factors.csv")

  # compile components for attributeList of dataTable

  # condition: factors present
  if (file.exists(rasterFactorsFileName)) {

    rasterAttributes <- data.frame(
      attributeName = "raster_value",
      attributeDefinition = rasterValueDescription
    )

    rasterFactors <- read_csv(rasterFactorsFileName,
                              col_types = cols()) # to suppress tibble output

    attr_list <- set_attributes(attributes = rasterAttributes, factors = rasterFactors, col_classes = "factor")

    # condition: factors not present
  } else {

    # do not proceed if the units for the rater values is not provided
    if (missing('rasterValueUnits')) { stop("please provide units for the raster cell values") }

    # determine raster number type
    # sample of raster values (10% of values sans NAs)
    rasterValuesSample <- na.omit(sample(rasterObject, size = 0.1 * ncell(rasterObject)))
    rasterValuesSample <- rasterValuesSample[is.finite(rasterValuesSample)] # remove infs (just in case)

    rounded <- floor(rasterValuesSample)

    if (length(rasterValuesSample) - sum(rasterValuesSample == rounded, na.rm = T) > 0) {

      rasterNumberType <- "real" # all

    } else if (min(rasterValuesSample, na.rm = T) > 0) {

      rasterNumberType <- "natural" # 1, 2, 3, ... (sans 0)

    } else if (min(rasterValuesSample, na.rm = T) < 0) {

      rasterNumberType <- "integer" # whole + negative values

    } else {

      rasterNumberType <- "whole" # natural + 0

    }

    rasterAttributes <- data.frame(
      attributeName = "raster_value",
      attributeDefinition = rasterValueDescription,
      unit = rasterValueUnits,
      numberType = rasterNumberType
    )

    attr_list <- set_attributes(attributes = rasterAttributes, col_classes = "numeric")

  }


  # add additionalInfo - projections ----------------------------------------

  if (missing(epsgProjection)) {

    if (emlProjection == "NAD_1983_UTM_Zone_12N") {

      epsgProjection <- 26912

    } else if (emlProjection == "NAD_1927_UTM_Zone_12N") {

      epsgProjection <- 26712

    } else if (emlProjection == "GCS_WGS_1984") {

      epsgProjection <- 4326

    } else if (emlProjection == "WGS_1984_UTM_Zone_12N") {

      epsgProjection <- 32612

    } else {

      projections <- list(
        section = list(
          paste0("<title>raster derived coordinate reference system</title>\n<para>", as.character(crs(rasterObject)), "</para>")
        )
      )

    }

  } else {

    projections <- list(
      section = list(
        paste0("<title>user provided coordinate reference system</title>\n<para>", epsgProjection,"</para>"),
        paste0("<title>raster derived coordinate reference system</title>\n<para>", as.character(crs(rasterObject)), "</para>")
      )
    )

  }


  # identify EML-compliant spatial reference --------------------------------

  if (missing(emlProjection)) {

    if (epsgProjection == 26912) {

      emlProjection <- "NAD_1983_UTM_Zone_12N"

    } else if (epsgProjection == 26712) {

      emlProjection <- "NAD_1927_UTM_Zone_12N"

    } else if (epsgProjection == 4326) {

      emlProjection <- "GCS_WGS_1984"

    } else if (epsgProjection == 32612) {

      emlProjection <- "WGS_1984_UTM_Zone_12N"

    } else {

      stop("cannot identify EML-compliant projection, contact package developer")

    }

  }


  # coverage ----------------------------------------------------------------

  spatialCoverage <- set_coverage(west = raster::extent(rasterObject)@xmin,
                                  east = raster::extent(rasterObject)@xmax,
                                  north = raster::extent(rasterObject)@ymax,
                                  south = raster::extent(rasterObject)@ymin)


  # create spatial raster entity --------------------------------------------

  # if zipping a directory

  if (zipFiles == TRUE) {

    # zip directory
    directoryNameFull <- sub("/$", "", path.expand(directoryName))
    zippedDirName <- paste0(directoryNameFull, ".zip")
    zipShell <- paste0("zip -jX ", zippedDirName, " ", directoryNameFull, "/*")
    system(zipShell)

    # rename zipped dir with md5sum
    zipHashName <- paste0(projectid, "_", file_path_sans_ext(basename(zippedDirName)), "_", md5sum(zippedDirName), ".zip")
    zipHashDirName <- paste0(dirname(directoryName), "/", zipHashName)
    renameShell <- paste0("mv ", zippedDirName, " ", zipHashDirName)
    system(renameShell)

    # build physical of zipped dir

    # set authentication (md5)
    fileAuthentication <- eml$authentication(method = "MD5")
    fileAuthentication$authentication <- md5sum(zipHashDirName)

    # set file size
    fileSize <- eml$size(unit = "byte")
    fileSize$size <- deparse(file.size(zipHashDirName))

    # set file format
    fileDataFormat <- eml$dataFormat(
      externallyDefinedFormat = eml$externallyDefinedFormat(formatName = "zip")
    )

    # set distribution
    fileDistribution <- eml$distribution(
      eml$online(url = paste0(baseURL, zipHashName))
    )

    # build physical
    spatialRasterPhysical <- eml$physical(
      objectName = zipHashName,
      authentication = fileAuthentication,
      size = fileSize,
      dataFormat = fileDataFormat,
      distribution = fileDistribution
    )

    newSR <- EML::eml$spatialRaster(
      entityName = zipHashName,
      entityDescription = description,
      physical = spatialRasterPhysical,
      coverage = spatialCoverage,
      additionalInfo = projections,
      attributeList = attr_list,
      spatialReference = EML::eml$spatialReference(
        horizCoordSysName = emlProjection
      ),
      numberOfBands = bandnr(rasterObject),
      rows = nrow(rasterObject),
      columns = ncol(rasterObject),
      cellSizeXDirection = xres(rasterObject),
      cellSizeYDirection = yres(rasterObject),
      id = zipHashName
    )

  } else {

    # if working with a raster file (i.e., not zipping a directory)

    newRasterName <- paste0(projectid, "_", basename(file_path_sans_ext(rasterFile)), "_", md5sum(rasterFile), ".", file_ext(rasterFile))
    newRasterNameDir <- paste0(directoryName, "/", newRasterName)

    file.copy(from = rasterFile,
              to = newRasterNameDir)

    # build physical of renamed raster

    # set authentication (md5)
    fileAuthentication <- eml$authentication(method = "MD5")
    fileAuthentication$authentication <- md5sum(newRasterNameDir)

    # set file size
    fileSize <- eml$size(unit = "byte")
    fileSize$size <- deparse(file.size(newRasterNameDir))

    # set file format
    fileDataFormat <- eml$dataFormat(
      externallyDefinedFormat = eml$externallyDefinedFormat(formatName = file_ext(newRasterNameDir))
    )

    # set distribution
    fileDistribution <- eml$distribution(
      eml$online(url = paste0(baseURL, newRasterName))
    )

    # build physical
    spatialRasterPhysical <- eml$physical(
      objectName = newRasterName,
      authentication = fileAuthentication,
      size = fileSize,
      dataFormat = fileDataFormat,
      distribution = fileDistribution
    )

    newSR <- EML::eml$spatialRaster(
      entityName = newRasterName,
      entityDescription = description,
      physical = spatialRasterPhysical,
      coverage = spatialCoverage,
      additionalInfo = projections,
      attributeList = attr_list,
      spatialReference = EML::eml$spatialReference(
        horizCoordSysName = emlProjection
      ),
      numberOfBands = bandnr(rasterObject),
      rows = nrow(rasterObject),
      columns = ncol(rasterObject),
      cellSizeXDirection = xres(rasterObject),
      cellSizeYDirection = yres(rasterObject),
      id = newRasterName
    )

  }

  # return spatial raster object --------------------------------------------

  return(newSR)


} # close create_spatialRaster
