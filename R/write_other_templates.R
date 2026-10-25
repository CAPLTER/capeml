#' @title Create a template yaml file for supplying attribute metadata for a
#' data object that cannot be read into R
#'
#' @description \code{write_other_attributes} creates a template as a yaml file
#' for supplying attribute metadata for an otherEntity (e.g., a zip file of
#' LiDAR point clouds in LAZ format) that cannot be read into the R
#' environment. Unlike
#' \code{write_attributes}, which derives a template from a data object in R,
#' the names and classes of the attributes are provided by the user.
#'
#' @details The template is named for the target file without its extension
#' (e.g., lidar_tiles.zip yields lidar_tiles_attrs.yaml) and written to
#' \code{metadata_path}. \code{create_otherEntity} reads the annotated template
#' and adds the metadata to the attributeList of the resulting otherEntity.
#' The EML schema permits an attributeList for any otherEntity regardless of
#' file format.
#'
#' @section When to use:
#' Attributes describe variables or fields that exist within the data object,
#' such as the point record fields (e.g., X, Y, Z, Classification) of LiDAR
#' point clouds in a zip file, the attribute table of a zipped shapefile, the
#' value classes of a raster, or the columns of spreadsheets in a zip file.
#' Each attribute must correspond to a variable in the object.
#'
#' @section Required fields by column class:
#' Every attribute requires an \code{attributeDefinition}. Additional fields
#' depend on \code{columnClasses}:
#' \describe{
#'   \item{numeric}{\code{unit} (an EML standard unit, see
#'   \code{EML::get_unitList()}, or a custom unit, which is documented in
#'   custom_units.yaml when the EML is built) and \code{numberType} (one of
#'   natural, whole, integer, real). \code{minimum} and \code{maximum} are
#'   optional and must be entered manually.}
#'   \item{character}{no additional fields; leave \code{definition} blank as
#'   it is populated from \code{attributeDefinition}.}
#'   \item{factor}{the codes and their definitions are provided in a factors
#'   template (see \code{\link{write_other_factors}}).}
#'   \item{Date}{\code{formatString} describing the format of the date or time
#'   (e.g., YYYY-MM-DD, YYYY, hh:mm:ss).}
#' }
#' The \code{propertyURI}, \code{propertyLabel}, \code{valueURI}, and
#' \code{valueLabel} fields are optional semantic annotations.
#' \code{create_otherEntity} stops and lists any missing required fields.
#'
#' @section What is not allowed:
#' Attributes are not a place for metadata about the file itself (e.g.,
#' author, number of files in an archive, version, software used to create
#' it); provide
#' those details in the \code{description} or \code{additional_information}
#' arguments of \code{create_otherEntity}, the methods, or dataset-level
#' metadata. Do not invent attributes that do not exist in the object.
#'
#' @section Limitations:
#' Because the data are not read, \code{minimum} and \code{maximum} cannot be
#' calculated, and missing value codes are not detected or supported in these
#' templates.
#'
#' @param target_file
#'  (character) The quoted name of the data file (e.g., "lidar_tiles.zip").
#'  The file is used only to name the template; it does not need to be
#'  readable.
#' @param attributes
#'  (character) The names of the attributes (variables) in the data object.
#' @param column_classes
#'  (character) The class of each attribute: numeric, character, factor, or
#'  Date. A single value applies to all attributes. Defaults to character.
#' @param overwrite
#'  (logical) Logical indicating if an existing attributes file in the target
#'  directory should be overwritten.
#' @param metadata_path
#'  (character) Directory to which the template is written. Defaults to
#'  \code{metadata_path} in config.yaml, else the working directory.
#'
#' @importFrom purrr map2
#' @importFrom stats setNames
#' @importFrom tools file_path_sans_ext
#'
#' @return The path to the template is returned invisibly, and a template for
#' providing attribute metadata is written to \code{metadata_path}.
#'
#' @examples
#' \dontrun{
#'
#'  # point record fields of the LiDAR point clouds (LAZ) in a zip file
#'
#'  capeml::write_other_attributes(
#'    target_file    = "lidar_tiles.zip",
#'    attributes     = c("X", "Y", "Z", "Classification", "GpsTime"),
#'    column_classes = c("numeric", "numeric", "numeric", "factor", "numeric")
#'  )
#'
#' }
#'
#' @export
#'
write_other_attributes <- function(
  target_file,
  attributes,
  column_classes = "character",
  overwrite      = FALSE,
  metadata_path  = NULL
  ) {

  # required parameters -------------------------------------------------------

  if (missing("target_file")) {

    stop("please provide the name of the target file")

  }

  if (missing("attributes") || length(attributes) == 0 || any(is.na(attributes) | attributes == "")) {

    stop("please provide the names of the attributes in ", target_file)

  }

  if (anyDuplicated(attributes) > 0) {

    stop("attribute names must be unique: ", paste(unique(attributes[duplicated(attributes)]), collapse = ", "))

  }

  allowed_classes <- c("numeric", "character", "factor", "Date")

  if (!all(column_classes %in% allowed_classes)) {

    stop(
      "column_classes must be one of ", paste(allowed_classes, collapse = ", "),
      "; not: ", paste(setdiff(column_classes, allowed_classes), collapse = ", ")
    )

  }

  if (length(column_classes) == 1) {

    column_classes <- rep(column_classes, length(attributes))

  } else if (length(column_classes) != length(attributes)) {

    stop("provide one column class for all attributes or one for each attribute")

  }


  # target file ---------------------------------------------------------------

  entity_name <- tools::file_path_sans_ext(basename(target_file))
  file_name   <- template_target(entity_name, "attrs", metadata_path)

  if (file.exists(file_name) && !isTRUE(overwrite)) {

    stop(
      paste0("file ", file_name, " already exists, use write_other_attributes(overwrite = TRUE) to overwrite")
    )

  }


  # construct template from empty vectors of each class -----------------------

  prototypes <- list(
    numeric   = numeric(0),
    character = character(0),
    factor    = factor(character(0)),
    Date      = as.Date(character(0))
  )

  attribute_list <- purrr::map2(
    .x = prototypes[column_classes],
    .y = attributes,
    .f = attributes_to_yaml
  ) |>
    stats::setNames(attributes)

  # the format of a date cannot be inferred from a file that is not read
  attribute_list <- purrr::map(
    .x = attribute_list,
    .f = function(attribute) {
      if (identical(attribute[["columnClasses"]], "Date")) {
        attribute[["formatString"]] <- ""
      }
      attribute
    }
  )

  write_attribute_yaml(attribute_list, file_name)
  prepend_template_header(file_name, "write_other_attributes")

  message(paste0("constructed attribute yaml: ", file_name))

  return(invisible(file_name))

}


#' @title Create a template yaml file for supplying factor metadata for a
#' data object that cannot be read into R
#'
#' @description \code{write_other_factors} creates a template as a yaml file
#' for supplying the codes and definitions of categorical attributes of an
#' otherEntity (e.g., a zip file of LiDAR point clouds in LAZ format) that
#' cannot be read into the R environment.
#'
#' @details The template is named for the target file without its extension
#' (e.g., lidar_tiles.zip yields lidar_tiles_factors.yaml) and written to
#' \code{metadata_path}. Each attribute of class factor in the attributes
#' template (see \code{\link{write_other_attributes}}) must have an entry in
#' the factors template, and every code requires a definition. Codes that are
#' not known when the template is created are written as a single blank level
#' to be completed (add further levels as needed). If a factors template is
#' provided without an attributes template, \code{create_otherEntity} documents
#' a single attribute using the \code{entity_value_description} argument (e.g.,
#' the value classes of a raster).
#'
#' @inheritSection write_other_attributes When to use
#' @inheritSection write_other_attributes What is not allowed
#'
#' @param target_file
#'  (character) The quoted name of the data file (e.g., "lidar_tiles.zip").
#'  The file is used only to name the template; it does not need to be
#'  readable.
#' @param factors
#'  (list) A named list in which each name is a categorical attribute and each
#'  element is a character vector of the codes of that attribute; use
#'  \code{character(0)} if the codes are not yet known.
#' @param overwrite
#'  (logical) Logical indicating if an existing factors file in the target
#'  directory should be overwritten.
#' @param metadata_path
#'  (character) Directory to which the template is written. Defaults to
#'  \code{metadata_path} in config.yaml, else the working directory.
#'
#' @importFrom purrr imap
#' @importFrom yaml as.yaml write_yaml
#'
#' @return The path to the template is returned invisibly, and a template for
#' providing code definitions is written to \code{metadata_path}.
#'
#' @examples
#' \dontrun{
#'
#'  # LAS classification codes of the LiDAR point clouds (LAZ) in a zip file;
#'  # use character(0) for a factor whose codes are not yet known
#'
#'  capeml::write_other_factors(
#'    target_file = "lidar_tiles.zip",
#'    factors     = list(
#'      Classification = c("1", "2", "3", "4", "5", "6", "7", "17")
#'    )
#'  )
#'
#' }
#'
#' @export
#'
write_other_factors <- function(
  target_file,
  factors,
  overwrite     = FALSE,
  metadata_path = NULL
  ) {

  # required parameters -------------------------------------------------------

  if (missing("target_file")) {

    stop("please provide the name of the target file")

  }

  if (
    missing("factors") || !is.list(factors) || length(factors) == 0 ||
      is.null(names(factors)) || any(names(factors) == "")
    ) {

    stop("factors must be a named list of the codes of each categorical attribute")

  }

  if (anyDuplicated(names(factors)) > 0) {

    stop("factor names must be unique: ", paste(unique(names(factors)[duplicated(names(factors))]), collapse = ", "))

  }

  duplicate_codes <- names(factors)[purrr::map_lgl(factors, ~ anyDuplicated(.x) > 0)]

  if (length(duplicate_codes) > 0) {

    stop("codes must be unique within each factor: ", paste(duplicate_codes, collapse = ", "))

  }


  # target file ---------------------------------------------------------------

  entity_name <- tools::file_path_sans_ext(basename(target_file))
  file_name   <- template_target(entity_name, "factors", metadata_path)

  if (file.exists(file_name) && !isTRUE(overwrite)) {

    stop(
      paste0("file ", file_name, " already exists, use write_other_factors(overwrite = TRUE) to overwrite")
    )

  }


  # construct template --------------------------------------------------------

  factor_list <- purrr::imap(
    .x = factors,
    .f = function(codes, attribute_name) {

      codes <- as.character(codes)

      if (length(codes) == 0) {
        codes <- ""
      }

      list(
        attribute = list(
          attributeName = attribute_name,
          levels        = purrr::map(codes, ~ list(code = .x, definition = ""))
        )
      )

    }
  ) |>
    unname()

  yaml::write_yaml(
    x    = yaml::as.yaml(factor_list),
    file = file_name
  )
  prepend_template_header(file_name, "write_other_factors")

  message(paste0("constructed factors yaml: ", file_name))

  return(invisible(file_name))

}


#' @title Prepend guidance comments to a metadata template
#'
#' @noRd
#'
prepend_template_header <- function(file_name, function_name) {

  header <- c(
    paste0("# metadata template generated by capeml::", function_name, "()"),
    paste0("# complete all blank required fields; see ?capeml::", function_name),
    "# for the fields required by each column class and what is not allowed"
  )

  writeLines(c(header, readLines(file_name)), file_name)

}


#' @title Check that attribute and factor templates are complete
#'
#' @description \code{check_attribute_templates} stops with a list of every
#' missing or invalid required field in the attribute and factor metadata of a
#' data entity so that incomplete templates do not yield EML that is not
#' compliant with the schema.
#'
#' @param attrs
#'  (data.frame) Attributes as returned by \code{read_attributes_file}, or NULL.
#' @param classes
#'  (character) Column classes as returned by \code{read_attributes_file}.
#' @param factors
#'  (data.frame) Factors as returned by \code{read_factors_file}, or NULL.
#' @param source
#'  (character) Name of the data entity for error messages.
#'
#' @noRd
#'
check_attribute_templates <- function(
  attrs,
  classes,
  factors,
  source
  ) {

  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

  field <- function(df, name) {
    if (name %in% names(df)) as.character(df[[name]]) else rep(NA_character_, nrow(df))
  }

  problems <- character(0)

  if (!is.null(attrs)) {

    attribute_names <- field(attrs, "attributeName")

    duplicates <- unique(attribute_names[duplicated(attribute_names)])
    if (length(duplicates) > 0) {
      problems <- c(problems, paste0("duplicate attributeName: ", paste(duplicates, collapse = ", ")))
    }

    allowed_classes <- c("numeric", "character", "factor", "Date")
    number_types    <- c("natural", "whole", "integer", "real")

    unit          <- field(attrs, "unit")
    number_type   <- field(attrs, "numberType")
    format_string <- field(attrs, "formatString")
    definition    <- field(attrs, "attributeDefinition")

    attribute_problems <- purrr::map(
      .x = seq_along(attribute_names),
      .f = function(i) {
        c(
          if (blank(definition[[i]])) "attributeDefinition is blank",
          if (!classes[[i]] %in% allowed_classes) {
            paste0("columnClasses '", classes[[i]], "' is not one of ", paste(allowed_classes, collapse = ", "))
          },
          if (classes[[i]] == "numeric" && blank(unit[[i]])) "unit is blank",
          if (classes[[i]] == "numeric" && !number_type[[i]] %in% number_types) {
            paste0("numberType must be one of ", paste(number_types, collapse = ", "))
          },
          if (classes[[i]] == "Date" && blank(format_string[[i]])) "formatString is blank",
          if (classes[[i]] == "factor" && !attribute_names[[i]] %in% factors[["attributeName"]]) {
            "no codes in the factors file"
          }
        )
      }
    )

    problems <- c(
      problems,
      unlist(purrr::map2(attribute_names, attribute_problems, ~ if (length(.y) > 0) paste0(.x, ": ", .y)))
    )

    if (!is.null(factors)) {

      unmatched <- setdiff(unique(factors[["attributeName"]]), attribute_names[classes == "factor"])
      if (length(unmatched) > 0) {
        problems <- c(
          problems,
          paste0(unmatched, ": in the factors file but not an attribute of class factor")
        )
      }

    }

  }

  if (!is.null(factors)) {

    blank_codes       <- unique(factors[["attributeName"]][blank(factors[["code"]])])
    blank_definitions <- unique(factors[["attributeName"]][blank(factors[["definition"]])])

    problems <- c(
      problems,
      if (length(blank_codes) > 0) paste0(blank_codes, ": factor code is blank"),
      if (length(blank_definitions) > 0) paste0(blank_definitions, ": factor definition is blank")
    )

  }

  if (length(problems) > 0) {

    stop(
      "incomplete attribute or factor metadata for '", source, "':\n",
      paste0("  - ", problems, collapse = "\n")
    )

  }

  invisible(TRUE)

}
