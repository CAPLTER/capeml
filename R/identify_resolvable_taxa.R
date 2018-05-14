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
identify_resolvable_taxa <- function(taxa_list) {

  resolved_taxa <- data.frame(taxon = NA, resolve = NA)

  for (i in 1:length(taxa_list)){
    info <- suppressWarnings(get_tsn(searchterm = taxa_list[i],
                                     searchtype = "scientific",
                                     accepted = T,
                                     ask = T))
    resolved_taxa[i,"taxon"] <- taxa_list[i]
    resolved_taxa[i,"resolve"] <- info[1]

  }

  return(resolved_taxa)

}
