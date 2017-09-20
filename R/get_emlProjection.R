#' @title get_emlProjection
#'
#' @description get_emlProjection attempts to identify the projection of a
#'   spatial data file, and match this to the corresponding projection ID
#'   permissible by EML.
#'
#' @details get_emlProjection is a helper function designed primarily to assist
#'   the creation of EML spatial data objects. The function currently is
#'   restricted to matching coordinate systems in the northern hemisphere, and
#'   will not match projections of type transverse mercator. Though intended
#'   primarily as a helper function, get_emlProjection can be run independently.
#'
#' @note get_emlProjection currently is restricted to matching coordinate
#'   systems in the northern hemisphere, and will not match projections of type
#'   transverse mercator.
#'
#' @param spatialDataEntity a spatial data entity, often loaded into R with the
#'   raster (for rasters) or rgdal (for vectors) packages
#'
#' @import rgdal
#' @importFrom stringr str_match str_replace_all
#' @import dplyr
#' @import magrittr
#' @importFrom readr read_csv
#' @importFrom raster crs
#'
#' @return if a suitable match was found, function returns an EML-compliant
#'   listing of the projection of the spatial data object
#'
#' @examples
#' \dontrun{
#' vectorData <- readOGR(dsn='/GISfiles/WatershedShapefile/', layer='AZwatersheds_prj')
#' rasterdata <- raster('CAP_1985.img')
#' emlCompliantProjection <- get_emlProjection(rasterdata)
#' }
#'
#' @export

get_emlProjection <- function(spatialDataEntity) {

  # will need a separate matching system for transverse mercator, which employs
  # lat, long, k, etc. as opposed to a datum and zone

  # parse individual components of the layer's projection
  entityProj <- paste0("+proj=", str_match(crs(spatialDataEntity), "(proj=)(\\w+)")[,3]) # projection
  entityZone <- paste0("+zone=", str_match(crs(spatialDataEntity), "(zone=)(\\w+)")[,3]) # zone
  entityDatum <- paste0("+datum=", str_match(crs(spatialDataEntity), "(datum=)(\\w+)")[,3]) # datum
  entityUnits <- paste0("+units=", str_match(crs(spatialDataEntity), "(units=)(\\w+)")[,3]) # units

  # generate a table of EPSG CRSs from rgdal package
  # filter table of EPSG CRSs based on CRS criteria of input file
  # this match is exclusive to Northern hemisphere references
  entityProjection <- rgdal::make_EPSG() %>%
    dplyr::filter(grepl(entityProj, prj4) & grepl(entityZone, prj4) & grepl(entityDatum, prj4) & grepl(entityUnits, prj4) & !grepl("+south", prj4)) %>%
    dplyr::mutate(code = as.integer(code)) %>%
    dplyr::mutate(note = as.character(note)) %>%
    dplyr::mutate(prj4 = as.character(prj4))

  # modif the EPSG text to facilitate matching with accepted EML CRS names
  entityProjectionString <- str_replace_all(entityProjection$note, "[^[:alnum:]] ", "") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("84", "1984") %>%
    str_replace_all("27", "1927")

  # return the EML accepted version of the CRS as a string
  return(unlist(emlCoordSystems[grepl(entityProjectionString, emlCoordSystems$coordSystemName, ignore.case = TRUE),], use.names = FALSE))

}
