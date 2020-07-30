#' @title create EML standard unit dictionary
#'
#' @description
#'     A convenience function to easily generate the EML standard unit
#'     dictionary as a data frame.
#'
#' @usage
#'     write_unit_dictionary()
#'
#' @export
#'

write_unit_dictionary <- function() {

  standardUnits <- EML::get_unitList()

  return(standardUnits)

}
