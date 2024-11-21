#' @title get the reference to a unit
#'
#' @description \code{get_unit_type} will determine the reference and details
#' (if relevant) of a supplied string as either qudt_en_us, qudt_en_uk, custom,
#' or from the EML standard dictionary. The latter returns \code{NULL}.
#'
#' @details The unit referece is determined by comparing the provided string
#' against catalogues of the EML standard unit dictionary, and subsets of the
#' QUDT unit dictionary. The QUDT unit dictionary is segregated between units
#' in the english US and UK variants. \code{get_unit_type} will try first to
#' match the supplied string against units in the QUDT subset of enlish (US)
#' units and seek a match in the UK variant if a US match is not found. Units
#' that are in the EML standard library return \code{NULL} value as further
#' documentation is not required for that set. \code{get_unit_type} is designed
#' as a helper function for other tools within \code{capeml} that involve units
#' but can be used independently.
#'
#' @param this_unit
#' (character) Quoted string of unit
#'
#' @importFrom EML get_unitList
#'
#' @return NULL if the provided string matches a reference in the EML standard
#' unit dictionary; a tibble of unit details if in the QUDT unit dictionary, or
#' a tibble of name and type fields if a custom unit (i.e., not in the EML
#' standard unit dictionary or the QUDT standard unit dicationary).
#'
#' @examples
#' \dontrun{
#' 
#' capeml::get_unit_type("dog_years")
#' #        name   type
#' # 1 dog_years custom
#' 
#' capeml::get_unit_type("C-M")
#' # A tibble: 1 × 4
#' # name    unit                           label         type      
#' # <chr>   <chr>                          <chr>         <chr>     
#' # 1 C-M   http://qudt.org/vocab/unit/C-M Coulomb Meter qudt_en_us
#' 
#' capeml::get_unit_type("meter")
#' # NULL
#'
#' }
#' #'
#'
#' @export
#'
get_unit_type <- function(this_unit) {

  qudt       <- FALSE
  standard   <- FALSE
  qudt_en_us <- FALSE

  if (this_unit %in% EML::get_unitList()[["units"]][["name"]]) {

    standard     <- TRUE
    unit_details <- NULL

  } else if (this_unit %in% capeml::qudt_table_en_us$name && standard == FALSE) {

    unit_details      <- capeml::qudt_table_en_us[capeml::qudt_table_en_us$name == this_unit, ]
    unit_details$type <- "qudt_en_us"
    qudt_en_us        <- TRUE
    qudt              <- TRUE

  } else if (this_unit %in% capeml::qudt_table_en_uk$name && standard == FALSE && qudt_en_us == FALSE) { 

    unit_details      <- capeml::qudt_table_en_uk[capeml::qudt_table_en_uk$name == this_unit, ]
    unit_details$type <- "qudt_en_uk"
    qudt              <- TRUE

  } else if (standard == FALSE && qudt == FALSE) { 

    unit_details <- data.frame(
      name = this_unit,
      type = "custom"
    )

  } else {

    message("could not identify unit reference for: ", this_unit)
    unit_details <- NULL

  }

  return(unit_details)

}
