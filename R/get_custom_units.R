#' @title translate custom and QUDT unit metadata to eml
#'
#' @description \code{get_custom_units} harvests metadata pertaining to units
#' of type custom and QUDT detailed in `custom_units.yaml`. Custom units, i.e.,
#' those not documented by QUDT or included in the EML standard libary, have a
#' id attribute and description element, where as QUDT units have only a id
#' attribute since their presence in the EML <unitList> is merely a placeholder
#' to accommodate EML congruence.
#'
#' @import EML
#' @importFrom yaml yaml.load_file
#' @importFrom purrr map
#'
#' @return EML eml entity is returned.
#'
#' @export
#'
get_custom_units <- function() {

  from_load <- yaml::yaml.load_file("custom_units.yaml")
  unit_list <- purrr::map(.x = from_load, ~ create_unit(.x))

  unit_list_eml <- list(
    metadata = list(
      unitList = list(
        unit = list(unit_list)
      )
    )
  )

  return(unit_list_eml)

}


#' @description \code{create_unit} is a helper function to
#' \code{get_custom_units} by packaging custom and QUDT metadata as type
#' EML::eml$unit.

create_unit <- function(unit_metadata) {

  unit_for_export <- EML::eml$unit(id = unit_metadata$name)

  if (exists("description", unit_metadata)) {

    unit_for_export$description <- unit_metadata$description

  }

  return(unit_for_export)

}
