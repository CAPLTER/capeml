#' @title write a project config.yaml file
#'
#' @description write_config writes a template config.yaml file to the working
#'  directory (default) or supplied path.
#'
#' @details A template config.yaml file is written to the working directory or
#'  specified location. The function requires that the package scope and number
#'  (e.g., "edi.521") are provided. Package identifiers include a verion
#'  number; the function uses a default value of version 1 but an alternate
#'  version number can be specified.
#'
#' @note The function requires specifically a 3-digit package number.
#'
#' @param packageScopeNumber
#'  (character) Quoted name of the package scope and number without the version
#'  number (e.g., "edi.521").
#' @param version
#'  (integer) Data package version (default = 1).
#' @param path
#'  (character) Path to where the config file will be written
#'  Defaults to the current directory.
#' @param overwrite (logical) Logical indicating if an existing config file in
#'  the target directory should be overwritten.
#'
#' @importFrom yaml write_yaml
#'
#' @export
#'
write_config <- function(
  packageScopeNumber,
  version = 1,
  path = ".",
  overwrite = FALSE) {

  # check if config.yaml already exists

  if (file.exists(paste0(path, "/", "config.yaml")) && overwrite == FALSE) {
    stop("config.yaml already exists, use `overwrite = TRUE` to overwrite")
  }

  # confirm packageScopeNumber is provided
  if (!exists("packageScopeNumber")) {
    stop("missing package id")
  }

  id <- regmatches(
    x = packageScopeNumber,
    m = regexpr(
      pattern = "\\d{2,}",
      text = packageScopeNumber,
      perl = TRUE)
  )

  id <- as.integer(id)

  if (nchar(id) != 3) {
    message("caution: project number is not the expected number of digits (3)")
  }

  fullIdentifier <- paste0(packageScopeNumber, ".", version)

  dataset_params <- list(
    packageNum = id,
    packageIdent = fullIdentifier,
    baseURL = "https://data.gios.asu.edu/datasets/cap/",
    project = "lter",
    title = "title",
    geographicCoverage = list(
      geographicDescription = "CAP LTER study area: greater Phoenix, Arizona (USA) metropolitan area and surrounding Sonoran desert region"
    )
  )

  yaml::write_yaml(
    x = dataset_params,
    file = paste0(path, "/", "config.yaml")
  )

  message(paste0("created config.yaml at ", path))

}
