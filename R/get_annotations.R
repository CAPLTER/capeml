#' @title translate annotations metadata to eml
#'
#' @description \code{get_annotations} harvests metadata pertaining to
#' annotations in a project `annotations.yaml` file and translates those
#' metadata to a list of type `EML::eml$annotation` for inclusion in a <eml>
#' element.
#'
#' @import EML
#' @importFrom yaml yaml.load_file
#' @importFrom purrr map
#'
#' @return `EML::eml$annotations`
#'
#' @export
#'
get_annotations <- function() {

  from_load <- yaml::yaml.load_file("annotations.yaml")
  annotation_list <- purrr::map(.x = from_load, ~ create_annotation(.x))

  # annotation_list <- list()

  # i <- 1

  # while (i <= length(from_load)) {

  #   annotation_list[[i]] <- create_annotation(from_load[[i]])
  #   i <- i + 1

  # }

  annotation_list <- list(
    annotation = annotation_list
  )

  return(annotation_list)

}

#' @description \code{create_annotation} is a helper function to
#' \code{get_annotations} that translates annotation metadata to a
#' `EML::eml$annotation` object.

create_annotation <- function(annotation_metadata) {

  property <- EML::eml$propertyURI(label = annotation_metadata$propertyLabel)
  property$propertyURI <- annotation_metadata$propertyURI

  value <- EML::eml$propertyURI(label = annotation_metadata$valueLabel)
  value$valueURI <- annotation_metadata$valueURI

  annotation_eml <- EML::eml$annotation(
    propertyURI = property,
    valueURI    = value
  )

  annotation_eml$references <- annotation_metadata$references

  return(annotation_eml)

}
