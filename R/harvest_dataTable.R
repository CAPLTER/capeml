#' @title Create metadata of type EML dataTable from existing dataTable
#' metadata in an xml file
#'
#' @description `harvest_dataTable` harvests EML metadata from existing
#' dataTable metadata in an xml file. The output is a EML entity of type
#' dataTable that can be included in a data package. The most common use case
#' is when including an existing dataTable in a new data package or one that is
#' being updated - `harvest_dataTable` allows adding the dataTable metadata to
#' the new or updated data package EML without re-creating the data entity.
#' This may be best explained with an examle. The CAP LTER maintains several
#' micrometeorological stations. Data from the long-running stations are
#' subsetted into approximately decadal intervals. Rather than rebuilding
#' historic data to include with package updates, `harvest_dataTable` allows
#' (re)constructing the metadata for those historic data entities for inclusion
#' in the package update without having to actually recreate the data entities.
#' Because the data are already published, all details (especially the hash)
#' are identical and the data files are accessible on AWS (even better: upload
#' can be skipped entirely for data that already loaded into EDI).
#'
#' @details `harvest_dataTable` is very much like `create_dataTable` in that it
#' generates an EML object with dataTable properties appropriate for inclusion
#' in a data package. However, rather than building the metadata as is the case
#' for `create_dataTable`, `harvest_dataTable` harvests existing dataTable
#' metadata (that reside in an xml file, the input to the function). Further,
#' `harvest_dataTable` does not generate or otherwise interact with data, or
#' attribute or factor metadata files.
#'
#' @note dataTable metadata must reside in an xml file that contains only
#' metadata pertaining to the dataTable of interest. That is,
#' `harvest_dataTable` will harvest metadata from an xml file that houses
#' metadata for a dataTable, but that file should contain only metadata
#' pertaining to that dataTable (`harvest_dataTable` is unable to harvest
#' metadata from a richer xml file that houses information beyond the dataTable
#' of interest).
#'
#' @param metadata_file
#' (character) The quoted path and name of the xml file that houses the
#' dataTable metadata.
#'
#' @import EML
#' @importFrom xml2 read_xml xml_find_all xml_text
#'
#' @return EML dataTable object is returned.
#'
#' @export
#'
harvest_dataTable <- function(metadata_file) {

  # do not proceed if the metadata_file parameter is not provided

  if (missing("metadata_file")) {

    stop("please provide a valid xml file and path (if relevant)")

  }

  # do not proceed if the provided file does not exist

  if (!file.exists(metadata_file)) {

    stop(paste0(metadata_file, " not found"))

  }

  eml_read_metadata <- EML::read_eml(metadata_file)
  xml_read_metadata <- xml2::read_xml(metadata_file)

  entity_name <- xml2::xml_find_all(xml_read_metadata, "entityName") |>
    xml2::xml_text()

  number_records <- xml2::xml_find_all(xml_read_metadata, "numberOfRecords") |>
    xml2::xml_text()

  entity_description <- xml2::xml_find_all(xml_read_metadata, "entityDescription") |>
    xml2::xml_text()

  reconstructed_data_table <- EML::eml$dataTable(
    entityName        = entity_name,
    entityDescription = entity_description,
    physical          = EML::eml_get(eml_read_metadata, "physical"),
    attributeList     = EML::eml_get(eml_read_metadata, "attributeList"),
    numberOfRecords   = number_records,
    id                = entity_name
  )

  if (length(EML::eml_get(eml_read_metadata, "coverage")) > 1) {

    reconstructed_data_table$coverage <- EML::eml_get(eml_read_metadata, "coverage")

  }

  return(reconstructed_data_table)

}
