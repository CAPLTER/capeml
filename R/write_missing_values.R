#' @title Construct table of missing values codes and their meaning
#'
#' @description The write_missing_values function reads an entity's attribute
#' details from a "entity name"_attrs.yaml or "entity name"_attrs.csv file in
#' the working directory - the details of which are incorporated into the EML
#' metadata for that entity. If present in the working directory, factor
#' metadata in a "entity name"_factors.yaml or "entity name"_factors.csv are
#' read and incorporated into attribute metadata.
#'
#' @details The write_missing_values function reads an entity's attribute
#' details from a "entity name"_attrs.yaml or "entity name"_attrs.csv file in
#' the working directory - the details of which are incorporated into the EML
#' metadata for that entity. If present in the working directory, factor
#' metadata in a "entity name"_factors.yaml or "entity name"_factors.csv are
#' read and incorporated into attribute metadata.
#'
#' @note The write_missing_values function is intended primarily as a helper to
#' other functions in the capeml ecosystem so is not meant to be called
#' directly (but can be).
#'
#' @param storage
#' (charcter) write_missing_values stores output in a tibble or dataframe. The
#' object should be emtpy and consist of three columns (attributeName
#' (character), code (character), and definition (charcater)). The user should
#' create this object prior to running the function and pass the unquoted name
#' of the object to write_missing_values via the storage parameter.
#' @param dataObject
#' (character) The unqouted name of the data entity that will be examined for
#' missing values.
#' @param field
#' (character) The quoted name of the field within the data entity that will be
#' examined for missing values.
#' @param MVC (optional)
#' (character) If relevant, the quoted name of a missing value code other than
#' NA or NaN that should be documented.
#'
#' @importFrom tibble tibble add_row
#'
#' @return A tibble documenting the presence of NA, NaN, or a user-specified
#' missing value code for the field of a tibble or dataframe.
#'
#' @examples
#' \dontrun{
#'
#'  missing_value_frame <- tibble::tibble(
#'    attributeName = as.character(),
#'    code = as.character(),
#'    definition = as.character()
#'  )
#'
#'  write_missing_values(
#'    storage = missingValueFrame,
#'    dataObject = captures,
#'    field = "weight",
#'    MVC = "X"
#'  )
#'
#'  # write_missing_values can be run on a single field (as above) of a data
#'  # object but the most common use case, and how it is applied in the capeml
#'  # ecosystem, is to loop over all fields in a data entity (sensu with purrr
#'  # below).
#'
#'  purrr::map_df(
#'    .x = colnames(data_entity),
#'    .f = capeml::write_missing_values,
#'    storage = missing_value_frame,
#'    dataObject = data_entity,
#'    MVC = "X"
#'  )
#'
#' }
#'
#' @export
#'
write_missing_values <- function(
  storage,
  dataObject,
  field,
  MVC) {

  if (any(is.na(dataObject[[field]]))) {

    storage <- storage %>%
      tibble::add_row(
        attributeName = field,
        code = "NA",
        definition = "missing value"
      )

  }

  if (any(is.nan(dataObject[[field]]))) {

    storage <- storage %>%
      tibble::add_row(
        attributeName = field,
        code = "NaN",
        definition = "missing value"
      )

  }

  if (any(dataObject[[field]] %in% MVC)) {

    storage <- storage %>%
      tibble::add_row(
        attributeName = field,
        code = MVC,
        definition = "missing value"
      )

  }

  return(storage)

}
