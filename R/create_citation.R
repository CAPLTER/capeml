#' @title create EML entity of type bibtex citation
#'
#' @description create_citation generates a EML entity of type bibtex citation
#'  that can be included in a EML literatureCited entity.
#'
#' @details create_citation generates a EML entity of type bibtex citation by
#'  harvesting publication details using the rcrossref library. The function
#'  accepts only a DOI. Citations can be manually constructed if a valid DOI is
#'  not available for the target material or if it is not indexed by crossref
#'  but this function is specific to refernces with valid DOIs.
#'
#' @param doi
#'  (character) quoted DOI of publication
#'
#' @importFrom rcrossref cr_cn
#'
#' @return EML citation entity is returned
#'
#' @examples
#' \dontrun{
#'
#' # Sartory, D.P., Grobbelaar, J.U. Extraction of chlorophyll a from
#' # freshwater phytoplankton for spectrophotometric analysis. Hydrobiologia 114,
#' # 177–187 (1984). https://doi.org/10.1007/BF00031869
#'
#' sartory <- create_citation("https://doi.org/10.1007/BF00031869")
#'
#' citations <- list(
#'   citation = list(
#'     sartory
#'   )
#' )
#'
#' dataset$literatureCited <- citations
#'
#' }
#'
#' @export

create_citation <- function(doi) {

  suppressWarnings(

    bibtex_citation <- cr_cn(
      dois = doi,
      format = "bibtex"
    )

  )

  if (!is.null(bibtex_citation)) {

    eml_citation <- EML::eml$citation(id = doi)
    eml_citation$bibtex <- bibtex_citation

    message("citation created")
    return(eml_citation)

  } else {

    message("could not resolve DOI")
    return(NULL)

  }

}
