#' @title write_keywords
#'
#' @description write_keywords creates a template as a csv file for supplying
#'   dataset keywords. The template csv file is pre-populated with CAP
#'   LTER-specific keywords attached to every data set, and inludes the relevant
#'   thesaurii (LTER controlled vocabular, LTER core areas, Creator Defined
#'   Keyword Set, CAPLTER Keyword Set List). The keywords.csv file has the
#'   appropriate structure to be read into the create_keywordSet function to
#'   include in EML metadata for a dataset.
#'
#' @param overwrite Logical indicating if an existing keywords.csv file in the
#'   target directory should be overwritten.
#'
#' @import dplyr
#' @import tibble
#' @importFrom readr write_csv
#'
#' @return output is a csv file titled keywords.csv
#'
#' @examples
#' \dontrun{
#'
#'  write_keywords()
#'
#' }
#'
#' @export

write_keywords <- function(overwrite = FALSE) {

  if(file.exists('keywords.csv') && overwrite == FALSE) { stop("file keywords.csv already exists, use write_keywords(overwrite = TRUE) to overwrite") }

  tibble(
    thesaurus = c(
      "LTER controlled vocabulary",
      "LTER core areas",
      "Creator Defined Keyword Set",
      "CAPLTER Keyword Set List",
      "CAPLTER Keyword Set List",
      "CAPLTER Keyword Set List",
      "CAPLTER Keyword Set List",
      "CAPLTER Keyword Set List",
      "CAPLTER Keyword Set List",
      "CAPLTER Keyword Set List"),
    keyword = c(
      NA,
      NA,
      NA,
      "cap lter",
      "cap",
      "caplter",
      "central arizona phoenix long term ecological research",
      "arizona",
      "az",
      "arid land"
    ),
    type = c(
      NA,
      NA,
      NA,
      "theme",
      "theme",
      "theme",
      "theme",
      "place",
      "place",
      "theme"
    )
  ) %>% write_csv('keywords.csv')

}
