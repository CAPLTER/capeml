#' @title Create a list of taxonomic classifications
#'
#' @description This function generates a list of taxonomic classification
#' records where each record is a single-rank list corresponding to the value
#' in the \code{taxonRank} column of the input data frame. The output structure
#' is designed to match the EML (Ecological Metadata Language)
#' \code{classifications} format, with each element containing only the
#' specified rank, its value, and (if present) associated common names and
#' taxon identifiers.
#'
#' @param taxa_df A data frame containing taxonomic information. Must include
#' at least the columns \code{taxonRank}, \code{taxa_clean} (the value for the
#' specified rank), \code{taxonID} (e.g., "ITIS:12345" or "GBIF:67890"), and
#' optionally \code{vernacularName} (common names, possibly comma- or
#' semicolon-separated).
#'
#' @importFrom purrr pmap
#' @importFrom stringr str_split str_detect
#' @importFrom dplyr case_when
#'
#' @return A list of lists:
#'   \itemize{
#'     \item{\code{taxonRankName}: The rank name (from \code{taxonRank}).}
#'     \item{\code{taxonRankValue}: The value for that rank (from
#'     \code{taxa_clean}).} \item{\code{commonName}: A list of common names (if
#'     present in \code{vernacularName}); omitted if not present.}
#'     \item{\code{taxonId}: A list with \code{provider} and \code{taxonId} }
#'   }
#'
#' @details Only the rank specified in \code{taxonRank} is included for each
#' record. The function is robust to missing or empty \code{vernacularName} and
#' \code{taxonID} fields. The output is suitable for use in EML metadata or
#' other workflows requiring a flat, single-rank taxonomic classification
#' structure.
#'
#' @examples
#' \dontrun{
#'   taxa_df <- data.frame(
#'     taxonRank = c("family", "genus"),
#'     taxa_clean = c("Felidae", "Panthera"),
#'     taxonID = c("ITIS:183833", "ITIS:180544"),
#'     vernacularName = c("cats", "big cats")
#'   )
#'   taxon_trees <- create_taxa_list(taxa_df)
#'   str(taxon_trees[[1]], max.level = 2)
#' }
#'
#' @export
#'
create_taxa_list <- function(taxa_df) {

  # helper: parse a taxonid string into provider and id components
  parse_taxon_id <- function(taxonID) {
    if (
      is.null(taxonID) || is.na(taxonID) || !stringr::str_detect(taxonID, ":")
    ) {
      return(list())
    }
    parts <- stringr::str_split(taxonID, ":", n = 2)[[1]]
    provider <- dplyr::case_when(
      tolower(parts[1]) == "itis" ~ "https://itis.gov",
      tolower(parts[1]) == "gbif" ~ "https://gbif.org",
      TRUE ~ parts[1]
    )
    list(provider = provider, taxonId = parts[2])
  }

  # helper: parse a common name string into a list of names
  parse_common_names <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(x)) {
      return(list())
    }
    names <- unlist(stringr::str_split(x, "\\s*[,;]\\s*"))
    as.list(names)
  }

  taxon_list <- purrr::pmap(
    taxa_df,
    function(...) {
      row <- list(...)
      this_rank <- as.character(row[["taxonRank"]])
      # only proceed if the rank column exists and is non-empty
      if (is.null(this_rank) || is.na(this_rank) || !nzchar(this_rank)) {
        return(NULL)
      }
      # only use the value in the taxonrank column
      rank_value <- as.character(row[["taxa_clean"]])
      if (is.null(rank_value) || is.na(rank_value) || !nzchar(rank_value)) {
        return(NULL)
      }
      out <- list(
        taxonRankName = this_rank,
        taxonRankValue = rank_value
      )
      # add commonname if present
      if ("vernacularName" %in% names(row)) {
        out$commonName <- parse_common_names(row[["vernacularName"]])
      }
      # add taxonid if present
      taxid <- parse_taxon_id(row[["taxonID"]])
      if (length(taxid) > 0) {
        out$taxonId <- taxid
      }
      # return as a single-item named list to match classifications structure
      setNames(list(out), "1")
    }
  )

  return(taxon_list)

}
