#' get_emlProjection
#'
#' @param spatialDataEntity a spatial data file
#'
#' @import rgdal
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @import readr
#'
#' @return if a suitable match was found, function returns an EML-compliant
#'  listing of the projection of the spatial file
#'
#' @export
#'

# generate a table of EPSG CRSs from rgdal package
EPSG <- make_EPSG()

# load file with the list of EML-acceptable projection names
emlCoordSystems <- read_csv('emlCoordSystems')

get_emlProjection <- function(spatialDataEntity) {

  # will need a separate matching system for transverse mercator, which employs
  # lat, long, k, etc. as opposed to a datum and zone

  # parse individual components of the layer's projection
  entityProj <- paste0("+proj=", str_match(crs(spatialDataEntity), "(proj=)(\\w+)")[,3]) # projection
  entityZone <- paste0("+zone=", str_match(crs(spatialDataEntity), "(zone=)(\\w+)")[,3]) # zone
  entityDatum <- paste0("+datum=", str_match(crs(spatialDataEntity), "(datum=)(\\w+)")[,3]) # datum
  entityUnits <- paste0("+units=", str_match(crs(spatialDataEntity), "(units=)(\\w+)")[,3]) # units

  # filter table of EPSG CRSs based on CRS criteria of input file
  # this match is exclusive to Northern hemisphere references
  entityProjection <- EPSG %>%
    filter(grepl(entityProj, prj4) & grepl(entityZone, prj4) & grepl(entityDatum, prj4) & grepl(entityUnits, prj4) & !grepl("+south", prj4)) %>%
    mutate(code = as.integer(code)) %>%
    mutate(note = as.character(note)) %>%
    mutate(prj4 = as.character(prj4))

  # modif the EPSG text to facilitate matching with accepted EML CRS names
  entityProjectionString <- str_replace_all(entityProjection$note, "[^[:alnum:]] ", "") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("84", "1984") %>%
    str_replace_all("27", "1927")

  # return the EML accepted version of the CRS as a string
  return(unlist(emlCoordSystems[grepl(entityProjectionString, emlCoordSystems$coordSystemName, ignore.case = TRUE),], use.names = FALSE))

}
