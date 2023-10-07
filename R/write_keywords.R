#' @title write a keywords template file
#'
#' @description \code{write_keywords} creates a template as a csv file for
#' supplying dataset keywords. The template csv file is pre-populated with CAP
#' LTER-specific keywords attached to every data set, and inludes the relevant
#' thesaurii (LTER controlled vocabular, LTER core areas, Creator Defined
#' Keyword Set, CAPLTER Keyword Set List). The keywords.csv file has the
#' appropriate structure to be read into the create_keywordSet function to
#' include in EML metadata for a dataset.
#'
#' @param path
#'  (character) Path to where the file will be written. Defaults to the current
#'  directory.
#' @param overwrite
#'  (logical) Indicates to overwrite an existing file if one exists.
#'
#' @importFrom tibble tibble
#' @importFrom readr write_csv
#'
#' @return output is a partially populated csv file titled keywords.csv
#'
#' @examples
#'
#'  # write_keywords()
#'  # write_keywords(overwrite = TRUE)
#'
#' @export

write_keywords <- function(
  path      = ".",
  overwrite = FALSE
  ) {

  target_path_file <- paste0(path, "/keywords.csv")

  if (file.exists(target_path_file) && overwrite == FALSE) {
    stop(
      paste0(target_path_file, " already exists (use overwrite)")
    )
  }

  tibble::tibble(
    thesaurus = c(
      "LTER controlled vocabulary",
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
      "urban",
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
      "theme",
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
    ) |> readr::write_csv(target_path_file)

}
