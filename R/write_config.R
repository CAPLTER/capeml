#' @title write a project config.yaml file
#'
#' @description write_config writes a template config.yaml file to the working
#' directory (default) or supplied path.
#'
#' @details A template config.yaml file is written to the working directory or
#' specified location. The function requires that the package scope and number
#' (e.g., "edi", 521) are provided. Package identifiers include a verion
#' number; the function uses a default value of version 1 but an alternate
#' version number can be specified.
#'
#' @note The function expects specifically a 3-digit package identifier (number).
#'
#' @param scope
#'  (character) Quoted name of the package scope (e.g., "edi"). The default is
#'  "knb-lter-cap".
#' @param identifier
#'  (integer) Data package identifier (number).
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
    baseURL                = "https://data.gios.asu.edu/datasets/cap/",
    project                = "lter",
    title                  = "title with subject time place",
    maintenance            = "none",
    geographic_description = "CAP LTER study area: greater Phoenix, Arizona (USA) metropolitan area and surrounding Sonoran desert region"
  )


  # write parameters to config.yaml

  yaml::write_yaml(
    x    = dataset_params,
    file = paste0(path, "/", "config.yaml")
  )


  # messaging

  message(paste0("created config.yaml at ", path))

}
