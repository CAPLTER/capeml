#' @title write a project config.yaml file
#'
#' @description write_config writes a template config.yaml file to the working
#' directory (default) or supplied path.
#'
#' @details A template config.yaml file is written to the working directory or
#' specified location. The function requires that the package scope and number
#' (e.g., "edi", 521) are provided. By default, no package version is written
#' and capeml determines the next version from the EDI repository. Supplying
#' \code{version} hard-codes the version in config.yaml so that no EDI API
#' call is needed to determine it (see
#' \code{\link{read_package_configuration}}).
#'
#' @details Two optional keys may be added to config.yaml to identify data and
#' metadata that do not reside in the working directory (see
#' \code{\link{get_locations}}): \code{data_path}, one or more directories
#' that are searched (recursively, if needed) for data files; and
#' \code{metadata_path}, a directory to which attribute and factor templates
#' are written and from which they are read. The working directory is used if
#' these keys are not provided.
#'
#' @note The function expects specifically a 3-digit package identifier (number).
#'
#' @param scope
#'  (character) Quoted name of the package scope (e.g., "edi"). The default is
#'  "knb-lter-cap".
#' @param identifier
#'  (integer) Data package identifier (number).
#' @param version
#'  (integer) Optional data package version to hard-code in config.yaml.
#'  Defaults to \code{NULL} (version determined from the EDI repository).
#' @param path
#'  (character) Path to where the config file will be written. Defaults to the
#'  current directory.
#' @param overwrite 
#' (logical) Logical indicating if an existing config file in the target
#' directory should be overwritten.
#'
#' @importFrom yaml write_yaml
#'
#' @export
#'
write_config <- function(
  scope      = "knb-lter-cap",
  identifier,
  version    = NULL,
  path       = ".",
  overwrite  = FALSE
  ) {

  # check if config.yaml already exists

  if (file.exists(paste0(path, "/", "config.yaml")) && overwrite == FALSE) {
    stop("config.yaml already exists, use `overwrite = TRUE` to overwrite")
  }


  # do not proceed if a identifier is not provided

  if (missing("identifier")) {

    stop("write_config missing package identifier (number)")

  }


  # check that package identifier (name) is a expected length

  if (nchar(identifier) != 3) {

    message("caution: project identifier (number) is not the expected number of digits (3)")

  }


  # construct parameters

  dataset_params <- list(
    scope                  = scope,
    identifier             = as.integer(identifier),
    baseURL                = "https://storage.googleapis.com/asuke-aws-gios-data/datasets/cap/",
    project                = "lter",
    title                  = "title with subject time place",
    maintenance            = "none",
    geographic_description = "CAP LTER study area: greater Phoenix, Arizona (USA) metropolitan area and surrounding Sonoran desert region"
  )

  if (!is.null(version)) {
    dataset_params$version <- as.integer(version)
  }


  # write parameters to config.yaml

  yaml::write_yaml(
    x    = dataset_params,
    file = paste0(path, "/", "config.yaml")
  )


  # messaging

  message(paste0("created config.yaml at ", path))

}
