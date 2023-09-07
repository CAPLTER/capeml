#' @title EML standard unit dictionary data frame
#'
#' @description
#'     A convenience function to easily generate the EML standard unit
#'     dictionary as a data frame.
#'
#' @usage
#'     write_unit_dictionary()
#'

.Deprecated(
  new     = "EML::get_unitList",
  package = "EML",
  old     = as.character(sys.call(sys.parent()))[1L]
)

write_unit_dictionary <- function() {

  standardUnits <- EML::get_unitList()

  return(standardUnits)

}
