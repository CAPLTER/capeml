#' @title Drop the geometry column from a simple features object
#'
#' @description \code{drop_geometry} removes the active geometry column from a
#' simple features (sf) object and strips the sf class, returning a plain data
#' frame or tibble suitable for tabular-metadata workflows. If the input is not
#' an sf object it is returned unchanged.
#'
#' @note \code{drop_geometry} is an internal helper used by
#' \code{write_attributes}, \code{write_factors}, and \code{read_attributes} to
#' avoid a hard runtime dependency on \code{sf::st_drop_geometry()} in those
#' tabular workflows. It is not exported.
#'
#' @note The geometry column is identified via the \code{sf_column} attribute
#' of the object, which is set by the sf package when the object is created.
#' Only that column is removed; all other columns are preserved.
#'
#' @param x
#' An R object, typically a data frame, tibble, or simple features (sf) object.
#'
#' @return If \code{x} inherits from class \code{"sf"}, returns \code{x} as a
#' plain data frame or tibble with the active geometry column removed and the
#' \code{"sf"} class dropped. Otherwise returns \code{x} unchanged.
#'
drop_geometry <- function(x) {

  if (inherits(x, "sf")) {
    geom_col <- attr(x, "sf_column")
    if (!is.null(geom_col)) x[[geom_col]] <- NULL
    class(x) <- setdiff(class(x), "sf")
  }

  x

}
