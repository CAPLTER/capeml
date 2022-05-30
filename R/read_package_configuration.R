#' @title Read information from project configuration file ("config.yaml")
#'
#' @description The read_package_configuration function reads the project
#' configuration details from the config.yaml files that must exist in the
#' working directory. This is a internal, convenience function intended mostly
#' for the utility of other capeml functions but read_package_configuration can
#' be run independenty.
#'
#' @note This function if a bit of overkill in the sense that other capeml
#' functions (e.g., create_dataTable) could eaisily access the configuration details by
#' accesing the config.yaml file. The purpose here is mostly to accomodate the
#' evolving structure of the config.yaml structure. For example, capeml
#' formerly took a projectIdentifier that featured scope + dataset number +
#' version. Logic is included here to accommodate older config.yaml styles that
#' are better encapsulated into this single function rather than every place
#' configuration details are accessed.
#'
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom stringr str_extract
#'
#' @return A list of configuration details.
#'
#' @export
#'
read_package_configuration <- function() {

  # do not proceed if config.yaml is not present

  if (!file.exists("config.yaml")) {

    stop("config.yaml not found")

  }


  # load configuration details

  configurations <- yaml::yaml.load_file("config.yaml")


  # package scope

  if (exists("packageIdent", configurations)) {

    scope <- stringr::str_extract(
      string  = configurations[["packageIdent"]],
      pattern = "^[^\\.]*"
    )

  } else if (exists("scope", configurations)) {

    scope <- configurations[["scope"]]

  } else {

    stop("could not resolve package scope")

  }


  #  package identifier (number)

  if (exists("packageNum", configurations)) {

    identifier <- stringr::str_extract(
      string  = configurations[["packageNum"]],
      pattern = "[0-9]+"
    )

  } else if (exists("identifier", configurations)) {

    identifier <- configurations[["identifier"]]

  } else {

    stop("could not resolve package identifier (number)")

  }

  identifier <- as.integer(identifier)


  # baseURL | project | title | maintenance

  fileURL     <- configurations[["baseURL"]]
  title       <- configurations[["title"]]
  project     <- configurations[["project"]]
  maintenance <- configurations[["maintenance"]]


  # geographic description

  if (exists("geographicCoverage", configurations)) {

    geographic_description  <- configurations[["geographicCoverage"]][["geographicDescription"]]

  } else {

    geographic_description  <- configurations[["geographic_description"]]

  }


  # return

  list(
    scope                  = scope,
    identifier             = identifier,
    fileURL                = fileURL,
    title                  = title,
    project                = project,
    maintenance            = maintenance,
    geographic_description = geographic_description
  )

}
