#' @title Generate geographic coverage element(s) for one or more locations
#' from a simple features object
#'
#' @description create_geographic_coverage will generate a EML geographic
#' coverage, including geographicDescription and boundingCoordinates, from a
#' simple features (SF) object.
#'
#' @param sf_object
#' (character) Unquoted name of the simple features (SF) object in the R
#' environment
#' @param description
#' (character) Quoted (if passing a textual description) or unquoted (if
#' passing a reference to an object) pointer to the location description.
#'
#' @importFrom sf st_is_simple st_is_empty st_bbox
#'
#' @return A list with a structure corresponding to that expected of the EML
#' schema for a geographic description, and that can be included as part of a
#' EML coverage entity.
#'
#' @examples
#' \dontrun{
#'
#' sampling_sites <- tibble::tibble(
#'   site = c(
#'     "Olive",
#'     "Sunnyside Drive",
#'     "Sunburst Farms",
#'     "Calvary",
#'     "New River Road",
#'     "Table Mesa East",
#'     "Table Mesa West",
#'     "Moores Gulch"
#'     ),
#'   latitude = c(
#'     33.560378,
#'     33.592730,
#'     33.621183,
#'     33.601869,
#'     33.857829,
#'     33.973805,
#'     33.983873,
#'     33.999179
#'     ),
#'   longitude = c(
#'     -112.180969,
#'     -112.167620,
#'     -112.151316,
#'     -112.120515,
#'     -112.197051,
#'     -112.099295,
#'     -112.185413,
#'     -112.132538
#'   )
#'   ) |>
#' sf::st_as_sf(
#'   coords = c("longitude", "latitude"),
#'   crs    = 4326
#' )
#'
#' geographic_coverage <- split(
#'   x = sampling_sites,
#'   f = sampling_sites$site
#' ) |>
#' {\(site) purrr::map(.x = site, ~ capeml::create_geographic_coverage(.x,
#' description = .x$site))}() |> unname()
#'
#' begin_date <- "2019-04-30"
#' end_date   <- "2020-06-17"
#'
#' coverage <- list(
#'   temporalCoverage = list(
#'     rangeOfDates = list(
#'       beginDate = list(
#'         calendarDate = begin_date
#'         ),
#'       endDate = list(
#'         calendarDate = end_date
#'       )
#'     )
#'   ), # close temporalCoverage
#' geographicCoverage = geographic_coverage
#' ) # close coverage
#'
#' }
#'
#' @export
#'
create_geographic_coverage <- function(sf_object, description) {

  if (!sf::st_is_simple(sf_object)) { stop("function requires an sf object") }
  if (sf::st_is_empty(sf_object))   { stop("sf_object is empty") }

  sf_object_box <- sf::st_bbox(sf_object)

  geographic_coverage <- list(
    geographicDescription = description,
    boundingCoordinates = list(
      westBoundingCoordinate  = sf_object_box[["xmin"]],
      eastBoundingCoordinate  = sf_object_box[["xmax"]],
      northBoundingCoordinate = sf_object_box[["ymax"]],
      southBoundingCoordinate = sf_object_box[["ymin"]]
    )
  )

  return(geographic_coverage)

}
