#' @title Identify taxa within a list that can be matched to ITIS
#'
#' @description Provided with a list of taxa names, identify_resolvable_taxa
#'   generates a list of those taxa and the associated ITIS identifier if a
#'   match was identified. identify_resolvable_taxa is intended primarily as a
#'   helpful function for set_taxonomicCoverage but can be used independently.
#'
#' @details A list of provided taxa and associated ITIS identifier if a match
#'   could be found.
#'
#' @param taxa_list A list of taxa to validate
#' @param nameType Indiate whether the taxonomic nomenclature refers to common
#'   or scientific names
#'
#' @import taxize
#'
#' @return List of provided taxa and associated ITIS identifier if a match was
#'   found (NA if otherwise).
#'
#' @examples
#' \dontrun{
#'
#' resolvedTaxa <- create_keyword(c('Ephemeroptera', 'Plecoptera')
#' resolvedTaxa <- create_keyword(listOfTaxa)
#'
#' }
#'
#' @export
#'
identify_resolvable_taxa <- function(taxa_list, nameType) {

  # check for required parameters

  # do not proceed if the project id has not been identified in the working env
  if (missing('taxa_list')) { stop("missing project id") }

  # do not proceed if the project id has not been identified in the working env
  if (missing('nameType')) { stop("missing project id") }

  # confirm appropriate search type
  if (!grepl("sci|com", nameType)) { stop("nameType must be 'scientific' or 'common'")}


  # empty data frame to hold results
  resolved_taxa <- data.frame(taxon = NA,
                              resolve = NA)

  for (i in 1:length(taxa_list)) {

    info <- suppressWarnings(get_tsn(searchterm = taxa_list[i],
                                     searchtype = nameType,
                                     accepted = T,
                                     ask = T))
    resolved_taxa[i,"taxon"] <- taxa_list[i]
    resolved_taxa[i,"resolve"] <- info[1]

  }

  return(resolved_taxa)

}
