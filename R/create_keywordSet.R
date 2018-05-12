#' @title create_keywordSet
#'
#' @description create_keywordSet generates a EML entity of type
#'   keywordSet.
#'
#' @details A keywordSet entity is created from a single data file (.csv) with
#'   the fields: thesaurus, keyword, and type where type is an intended (but
#'   optional) attribute for the keyword.
#'
#' @param keywordsFile The quoted name and path of the keywords file.
#'
#' @import EML
#' @importFrom dplyr filter
#' @importFrom readr read_csv
#'
#' @return EML keywordSet object is returned.
#'
#' @examples
#' \dontrun{
#'
#' datasetKeywordSet <- create_keywordSet('keywordTest.csv')
#'
#' The workflow harvests keyword metadata from a keyword file (.csv) with the
#' columns: thesaurus, keyword, type where type is an optional attribute that
#' will be associated with the keyword EML/XML element.
#'
#' }
#'
#' @export
#'
create_keywordSet <- function(keywordsFile) {

  keywords <- read_csv(keywordsFile)

  thesaurusSet <- list()
  for (category in unique(keywords$thesaurus)) {

    keywordsSubset <- keywords %>%
      filter(thesaurus == category)

    keywordList <- list()
    for (word in unique(keywordsSubset$keyword)) {

      keywordIdent <- create_keyword(datasetKeyword = keywordsSubset[keywordsSubset$keyword == word,]$keyword,
                                     theAttribute = keywordsSubset[keywordsSubset$keyword == word,]$type)
      keywordList[[word]] <- keywordIdent

    } # close keywordList

    thesaurusSet[[category]] <- new('keywordSet',
                                    keyword = unname(keywordList))

    thesaurusSet[[category]]@keywordThesaurus <- category

  } # close thesaurusSet

  datasetKeywords <- unname(thesaurusSet)

  return(datasetKeywords)

}
