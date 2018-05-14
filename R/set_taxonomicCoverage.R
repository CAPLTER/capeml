#' @title Generate EML taxonomic coverage element from a list of resolvable taxa
#'
#' @description This is a stand-alone function borrowed from the rOpenSci EML
#'   package (https://github.com/ropensci/EML). Provided with a list of taxa
#'   names with resolvable ITIS identifiers, set_taxonomicCoverage generates a
#'   EML entity of type taxonomicCoverage for inclusion in an EML document.
#'
#' @details Input is a list of ITIS-resolvable taxa (identify_resolvable_taxa
#'   should be run first and set_taxonomicCoverage run on that list). Output is
#'   a EML entity of type taxonomicCoverage for those taxa.
#'
#' @param sci_names A list of dataset taxa
#' @param expand Flag: set to TRUE to use taxize to expand sci_names into full
#'   taxonomic classifications (FALSE is default)
#' @param db Information source for taxonomic details (ITIS is default)
#'
#' @import taxize
#' @import EML
#'
#' @return EML entity of type taxonomicCoverage
#'
#' @examples
#' \dontrun{
#'
#' taxaCoverage <- set_taxonomicCoverage(resolved_taxa, expand = T, db = 'itis')
#'
#' # set taxonomicCoverage element in EML coverage
#' # coverage@taxonomicCoverage <- c(taxaCoverage)
#'
#' }
#'
#' @export
#'
set_taxonomicCoverage <- function(sci_names, expand=FALSE, db = 'itis') {
  # Expand using taxize and ITIS if the user passes in just scientific names
  if (is(sci_names, "character") && expand) {
    if (!requireNamespace("taxize", quietly = TRUE)) {
      stop(call. = FALSE,
           "Expansion of scientific names requires the 'taxize' package to be installed. Install taxize or set expand to FALSE.")
    }

    classifications <- taxize::classification(sci_names, db = db)

    # Remove any NAs and warn for each
    if (any(is.na(classifications))) {
      warning(call. = FALSE,
              paste0("Some scientific names were not found in the taxonomic database and were not expanded: ", paste0(sci_names[which(is.na(classifications))], collapse = ","), "."))
    }

    # Turn result into a list of named lists where names are the rank name and
    # values are the rank value
    sci_names <- mapply(function(cls, sci_name) {
      # If the species name isn't found in the database, NA is returned
      if (is.list(cls)) {
        x <- as.list(cls[["name"]])
        names(x) <- cls[["rank"]]
        x
      } else {
        x <- list(list("species" = as.character(sci_name)))
        names(x) <- sci_name
        x
      }
    }, classifications, names(classifications), SIMPLIFY = FALSE)
  }

  if (class(sci_names) == "character") {
    taxa <- lapply(sci_names, function(sci_name) {
      s <- strsplit(sci_name, " ")[[1]]
      new(
        "taxonomicClassification",
        taxonRankName = "genus",
        taxonRankValue = s[[1]],
        taxonomicClassification = c(
          new(
            "taxonomicClassification",
            taxonRankName = "species",
            taxonRankValue = sci_name
          )
        )
      )
    })
    new("taxonomicCoverage",
        taxonomicClassification = do.call(c, taxa))
  } else if (class(sci_names) == "data.frame") {
    taxon_classification <- colnames(sci_names)
    new <- as.data.frame(t(sci_names))
    colnames(new) <- NULL
    taxa <- lapply(new, function(sci_name) {
      tc <- lapply(taxon_classification, function(name) {
        new(
          "taxonomicClassification",
          taxonRankName = name,
          taxonRankValue = as.character(sci_name[name])
        )
      })
      tc <- formRecursiveTree(tc)[[1]]
    })
    new("taxonomicCoverage",
        taxonomicClassification = do.call(c, taxa))
  } else if (class(sci_names) == "list") {
    # Warn if not a list of lists
    if (!all(vapply(sci_names, class, "") == "list")) {
      message("sci_names should be a list of lists. Your input was automatically wrapped up in a list.")
      sci_names <- list(sci_names)
    }

    taxa <- lapply(sci_names, function(sci_name) {
      taxonRankNames <- as.list(names(sci_name))

      taxa <- lapply(taxonRankNames, function(name) {
        new(
          "taxonomicClassification",
          taxonRankName = as.character(name),
          taxonRankValue = as.character(sci_name[[name]])
        )
      })
      formRecursiveTree(taxa)
    })

    new("taxonomicCoverage",
        taxonomicClassification = new("ListOftaxonomicClassification", do.call(c, taxa)))
  } else {
    stop("Incorrect format: sci_names can only be character string, data.frame or list")
  }
}

# helper function: form a nested tree recursively
formRecursiveTree <- function(listOfelements) {
  if (length(listOfelements) == 1 ||
      length(listOfelements) == 2 && is.null(listOfelements[[2]])) {
    return(do.call(c, listOfelements[1]))
  } else if (is.null(listOfelements[[1]])) {
    formRecursiveTree(listOfelements[2:length(listOfelements)])
  } else {
    listOfelements[[1]]@taxonomicClassification <- formRecursiveTree(listOfelements[2:length(listOfelements)])
    return(do.call(c, listOfelements[1]))
  }
}
