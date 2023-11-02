#' @title create or update data package with EDI API
#'
#' @description \code{create_package} creates or updates a data package in the
#' EDI data repository.
#'
#' @note \code{create_package} will look for a config.yaml file to estalish the
#' package scope and identifier. Alternatively, these parameters can be
#' provided by the user but, in that case, both parameters must be provided.
#'
#' @note The user must have established a connection to the EDI API (see
#' EDIutils::login) in the current R session.
#'
#' @param identifier
#'  (integer) Unquoted package identifier integer
#' @param scope
#'  (character) Quoted package scope (e.g., "knb-lter-cap", "edi")
#' @param environment
#'  (character) Quoted name of the EDI environment where the data package
#'  should be created, can be: production, staging (default), or development.
#' @param update
#'  (logical) Boolean value indicating if the data package already exists and
#'  should be updated.
#'
#' @import EDIutils
#'
#' @export
#'
create_package <- function(
  identifier  = NULL,
  scope       = NULL,
  environment = "staging",
  update      = FALSE
  ) {

  if (is.null(identifier) || is.null(scope)) {

    if (!file.exists("config.yaml")) {

      stop("config.yaml not found")

    }

    package_configs <- capeml::read_package_configuration()
    identifier      <- package_configs$identifier
    scope           <- package_configs$scope

  }

  version <- capeml::get_next_version(
    provided_scope      = scope,
    provided_identifier = identifier

  )

  if (update == TRUE) {

    EDIutils::update_data_package(
      eml         = paste(scope, identifier, version, "xml", sep = "."),
      useChecksum = TRUE,
      env         = environment

    )

  } else {

    EDIutils::create_data_package(
      eml = paste(scope, identifier, version, "xml", sep = "."),
      env = environment
    )

  }

}
