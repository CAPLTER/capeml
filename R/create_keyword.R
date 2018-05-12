#' @title create_keyword
#'
#' @description create_keyword generates a EML entity of type keyword with the
#'   option of adding an attribute to the keyword XML element. This is primarily
#'   inteneded as a helper function for the create_keywordSet function but can
#'   be run independently.
#'
#' @details A EML entity of type keyword is created from a string (keyword
#'   element) and optional string as the keyword attribute.
#'
#' @param datasetKeyword A keyword or subject.
#' @param theAttribute A keyword attribute.
#'
#' @import EML
#'
#' @return EML keyword object is returned.
#'
#' @examples
#' \dontrun{
#'
#' arizona <- create_keyword('Arizona', 'place')
#' avifauna <- create_keyword('avifauna', 'theme')
#' phoenix <- create_keyword('phoenix')
#'
#' }
#'
#' @export
#'
create_keyword <- function(datasetKeyword, theAttribute) {

  emlKeyword <- as(datasetKeyword, "keyword")

  if(!missing(theAttribute)) {
    emlKeyword@keywordType <- as(theAttribute, "xml_attribute")
  }

  return(emlKeyword)
}
