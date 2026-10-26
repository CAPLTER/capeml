#' @title Resolve the version of a data package
#'
#' @description Internal helper that returns the data package version for
#' functions that construct a package name (scope + identifier + version).
#'
#' @details A \code{version} hard-coded in config.yaml takes precedence; in
#' that case neither \code{\link{edi_login}} nor the EDI API is called. This
#' allows capeml workflows to run where the EDI API is a bottleneck or is not
#' reachable (e.g., ASU SOL). Without a hard-coded version, the next version is
#' determined from the EDI repository with \code{\link{get_next_version}}.
#'
#' @param configurations
#' (list) Configuration details from \code{\link{read_package_configuration}}.
#' @param display_message
#' (logical) Passed to \code{\link{get_next_version}}.
#'
#' @return integer reflecting the version of the data package
#'
#' @keywords internal
#' @noRd
#'
resolve_version <- function(
  configurations,
  display_message = FALSE
  ) {

  if (!is.null(configurations$version)) {

    message(
      "using version ", configurations$version,
      " from config.yaml; EDI repository not queried"
    )

    return(configurations$version)

  }

  capeml::get_next_version(
    provided_scope      = configurations$scope,
    provided_identifier = configurations$identifier,
    display_message     = display_message
  )

}
