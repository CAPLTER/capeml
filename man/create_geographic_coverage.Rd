% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_geographic_coverage.R
\name{create_geographic_coverage}
\alias{create_geographic_coverage}
\title{Generate geographic coverage element(s) for one or more locations
from a simple features object}
\usage{
create_geographic_coverage(sf_object, description)
}
\arguments{
\item{sf_object}{(character) Unquoted name of the simple features (SF) object in the R
environment}

\item{description}{(character) Quoted (if passing a textual description) or unquoted (if
passing a reference to an object) pointer to the location description.}
}
\value{
A list with a structure corresponding to that expected of the EML
schema for a geographic description, and that can be included as part of a
EML coverage entity.
}
\description{
create_geographic_coverage will generate a EML geographic
coverage, including geographicDescription and boundingCoordinates, from a
simple features (SF) object.
}
\examples{
\dontrun{

sampling_sites <- tibble::tibble(
  site = c(
    "Olive",
    "Sunnyside Drive",
    "Sunburst Farms",
    "Calvary",
    "New River Road",
    "Table Mesa East",
    "Table Mesa West",
    "Moores Gulch"
    ),
  latitude = c(
    33.560378,
    33.592730,
    33.621183,
    33.601869,
    33.857829,
    33.973805,
    33.983873,
    33.999179
    ),
  longitude = c(
    -112.180969,
    -112.167620,
    -112.151316,
    -112.120515,
    -112.197051,
    -112.099295,
    -112.185413,
    -112.132538
  )
  ) |>
sf::st_as_sf(
  coords = c("longitude", "latitude"),
  crs    = 4326
)

geographic_coverage <- split(
  x = sampling_sites,
  f = sampling_sites$site
) |>
{\(site) purrr::map(.x = site, ~ capeml::create_geographic_coverage(.x,
description = .x$site))}() |> unname()

begin_date <- "2019-04-30"
end_date   <- "2020-06-17"

coverage <- list(
  temporalCoverage = list(
    rangeOfDates = list(
      beginDate = list(
        calendarDate = begin_date
        ),
      endDate = list(
        calendarDate = end_date
      )
    )
  ), # close temporalCoverage
geographicCoverage = geographic_coverage
) # close coverage

}

}
