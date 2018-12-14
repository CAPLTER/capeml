#' @title create_keywordSet
#'
#' @description create_keywordSet generates a list of keywords and attributes
#'   that is suitable for passing to the keywordSet parameter of a eml$dataset
#'
#' @details A keywordSet entity is created from a single data file (.csv) with
#'   the fields: thesaurus, keyword, and type where type is an intended (but
#'   optional) attribute for the keyword.
#'
#' @param keywordsFile The quoted path and name of the keywords file.
#'
#' @import EML
#' @import purrr
#' @importFrom dplyr filter
#' @importFrom readr read_csv
#'
#' @return list of keywords and attributes that is suitable for passing to the
#'   keywordSet parameter of a eml$dataset
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

  keywordsFile <- read_csv(keywordsFile)

  keywordSet <- map(unique(keywordsFile[['thesaurus']]), create_keywordList, keywordsFile = keywordsFile)

  return(keywordSet)

}

# create keyword function: helper function in the create_keywordSet workflow.
# Generates a EML keyword with or without an attribute
create_keyword <- function(datasetKeyword, keywordAttribute = NA) {

  # require function input: keyword
  if (missing(datasetKeyword)) { stop("keyword required")}

  # generate keyword sans attribute
  if (is.na(keywordAttribute) || keywordAttribute == '') {

    emlKeyword <- datasetKeyword

  } else {

    # generate keyword with attribute
    emlKeyword <- eml$keyword(keywordType = keywordAttribute)
    emlKeyword$keyword <- datasetKeyword

  }

  return(emlKeyword)

}

# create keyword list function: helper function in the create_keywordSet
# workflow. Generates a list of keywords for a given thesaurus
create_keywordList <- function(category, keywordsFile) {

  thesaurusSubset <- keywordsFile %>%
    filter(thesaurus == category)

  keywordSubset <- map2(.x = thesaurusSubset$keyword, .y = thesaurusSubset$type, .f = create_keyword)

  emlKeywordList <- list(
    keyword = keywordSubset,
    keywordThesaurus = category
  )

  return(emlKeywordList)

}
