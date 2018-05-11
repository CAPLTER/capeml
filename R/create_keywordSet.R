#' @title create_keywordSet
#'
#' @description create_keywordSe generates a EML entity of type
#'   keywordSet
#'
#' @details a keywordSet entity is created from a single data file (.csv) with
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
#' need to add...
#'
#' The workflow features harvesting metadata from template files. Most metadata
#' are documented in a raster-level metadata file (e.g., CAP1985_metadata.csv
#' below). If raster values are categorical, an addiitonal metadata file is
#' required to document the categories (e.g., CAP1985_factors.csv below)
#'
#'  spatial_entity <- create_spatialRaster(
#'    pathToRaster = "~/folder/",
#'    metadataFile = "~/folder/CAP1985_metadata.csv",
#'    categoricalMetadataFile = "~/folder/CAP1985_factors.csv")
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

      keywordIdent <- generateKeyword(datasetKeyword = keywordsSubset[keywordsSubset$keyword == word,]$keyword,
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

#' @rdname create_keywordSet
#'
#' @description A helper function to generate EML entities of type keyword.
#'
#' @param datasetKeyword A keyword or subject.
#' @param theAttribute A keyword attribute.
#'
#' @return EML keyword object is returned.
#'
#' @export
#'
generateKeyword <- function(datasetKeyword, theAttribute) {

  emlKeyword <- as(datasetKeyword, "keyword")

  if(!missing(theAttribute)) {
    emlKeyword@keywordType <- as(theAttribute, "xml_attribute")
  }

  return(emlKeyword)
}
