#' @title Generate geographic coverage element(s) for one or more locations
#' from a simple features object
#'
#' @description \strong{Deprecated.} This function has moved to the
#' \code{capemlVector} package. Please install \code{capemlVector} and call
#' \code{capemlVector::create_geographic_coverage()} instead.
#'
#' @param sf_object ignored
#' @param description ignored
#'
#' @note This function was relocated to the \code{capemlVector} package to
#' allow the \code{capeml} package to remove its dependency on \code{sf}.
#' Install the replacement with:
#' \code{pak::pak("CAPLTER/capemlVector")}
#'
#' @return Stops with a deprecation error.
#'
#' @export
#'
create_geographic_coverage <- function(sf_object, description) {

  .Deprecated(
    new     = "create_geographic_coverage",
    package = "capemlVector",
    old     = as.character(sys.call(sys.parent()))[1L],
    msg     = paste0(
      "'create_geographic_coverage()' has moved to the capemlVector package.\n",
      "Install with: pak::pak(\"CAPLTER/capemlVector\")\n",
      "Then call:    capemlVector::create_geographic_coverage()"
    )
  )

  stop()

}

