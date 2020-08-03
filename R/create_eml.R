#' @title create_eml
#'
#' @description create_eml generates a EML entity of type eml
#'
#' @details A eml entity is created from objects defined in the user's R
#'   environment. The function loA project scope (default is LTER) indicates
#'   contact and project details specific to the research. The abstract and
#'   methods must be in markdown format - by default the package will look for
#'   these files (abstract.md, methods.md) in the project directory but files of
#'   different names or locadtions can be passed.
#'
#' @note create_eml will look for most inputs used to construct a eml entity,
#'   such as access and dataset, in the working environment; these parameters
#'   are not passed directly to the function and must exist in the working
#'   environment.
#'
#' @note Some parameters, such access, are loaded in the backgroud when the
#'   package is loaded and not called directly by the user.
#'
#' @import EML
#'
#' @return EML eml entity is returned.
#'
#' @export
#'
create_eml <- function() {

  # confirm required components exist in R environment
  if (!exists("lterAccess")) { stop("missing access") }
  if (!exists("dataset")) { stop("missing dataset") }

  # retrieve packageIdent from config.yaml
  if (!file.exists("config.yaml")) {
    stop("config.yaml not found")
  }
  packageIdent <- yaml::yaml.load_file("config.yaml")$packageIdent

  # construct eml
  eml <- EML::eml$eml(
    access = lterAccess,
    dataset = dataset,
    packageId = packageIdent,
    system = "knb",
    scope = "system"
  )

  # add custom units if relevant
  if (exists("unitList")) { eml$additionalMetadata  <- unitList }

  return(eml)

}
