#' @title Create taxonomicCoverage EML node from taxonomic data
#'
#' @description This function reads a taxa data file (taxa_df.csv) and
#' generates an EML-compliant taxonomicCoverage node, suitable for use with the
#' EML R package. It uses \code{create_taxa_list()} to generate a list of
#' single-rank taxonomic classification records, then wraps these using
#' \code{set_taxonomic_coverage()} to produce the EML taxonomicCoverage
#' structure. The resulting EML node is written to \code{taxonomicCoverage.xml}
#' in the working directory and returned as a list.
#'
#' @details The input taxa data file (\code{taxa_df.csv}) must contain at least
#' the columns \code{taxonRank}, \code{taxa_clean} (the value for the specified
#' rank), \code{taxonID} (e.g., "ITIS:12345" or "GBIF:67890"), and optionally
#' \code{vernacularName} (common names, possibly comma- or
#' semicolon-separated).
#'
#' Only the rank specified in \code{taxonRank} is included for each record. The
#' function is robust to missing or empty \code{vernacularName} and
#' \code{taxonID} fields. The output is suitable for use in EML metadata or
#' other workflows requiring a flat, single-rank taxonomic classification
#' structure.
#'
#' It is important to note that a taxonomic tree for each organism is not
#' generated. Rather, the function builds a `taxonomicClassification` element
#' for each taxon that includes only the derived rank of the organism. For
#' example, if the taxon \emph{Larrea tridentata} is submitted, the output will
#' include content (authority, authority ID, and common name (if relevant)) for
#' that species. That is, the result will not include content related to the
#' family, order, class, etc. to which the organism is associated. This
#' approach is in contrast to other tools, such as taxonomyCleanr, that build a
#' complete taxonomic tree.
#'
#'
#' @importFrom readr read_csv
#' @importFrom EML write_eml
#'
#' @return A list with a single element \code{taxonomicCoverage}, which is
#' itself a list of EML-compliant \code{taxonomicClassification} nodes. The
#' function also writes \code{taxonomicCoverage.xml} to the working directory.
#'
#' @seealso \code{\link{create_taxa_list}},
#' \code{\link{set_taxonomic_coverage}}
#'
#' @examples
#' \dontrun{
#'   # Assumes taxa_df.csv is present in the working directory
#'   coverage <- create_taxonomicCoverage()
#'   str(coverage, max.level = 3)
#' }
#'
#' @export
#'
create_taxonomicCoverage <- function() {

  message("creating <taxonomicCoverage>")

  if (!file.exists(paste0("taxa_map.csv"))) {
    stop("taxa_map.csv not found in the current working directory")
  }

  taxadb_output  <- readr::read_csv("taxa_df.csv")
  as_list        <- create_taxa_list(taxa_df = taxadb_output)
  classification <- set_taxonomic_coverage(sci_names = as_list)
  coverage       <- list(taxonomicCoverage = classification)

  EML::write_eml(
    eml  = coverage,
    file = "/taxonomicCoverage.xml"
  )

  return(coverage)

}


#' @title Create the taxonomicCoverage EML node
#'
#' @param sci_names
#' (list) Object returned by \code{capeml::create_taxa_list()}.
#'
#' @return
#' \item{list}{ a emld list object is returned for use with the EML R Package }
#' \item{.xml file}{ a .xml file is written to the working directory }
#'
#' @note courtesy of the taxonomyCleanr package
#'
#' @keywords internal
#'
set_taxonomic_coverage <- function(sci_names) {

  pop <- function(taxa) {
    if (length(taxa) > 1) {
      list(
        taxonRankName           = taxa[[1]]$taxonRankName,
        taxonRankValue          = taxa[[1]]$taxonRankValue,
        taxonId                 = taxa[[1]]$taxonId,
        commonName              = taxa[[1]]$commonName,
        taxonomicClassification = pop(taxa[-1])
      )
    } else {
      list(
        taxonRankName  = taxa[[1]]$taxonRankName,
        taxonRankValue = taxa[[1]]$taxonRankValue,
        taxonId        = taxa[[1]]$taxonId,
        commonName     = taxa[[1]]$commonName
      )
    }
  }

  taxa <- lapply(
    sci_names,
    function(sci_name) {
      pop(sci_name)
    }
  )

  return(list(taxonomicClassification = taxa))

}
