#' @title Load attribute information from a entity attribute yaml or csv file
#'
#' @description The \code{read_attributes_file} function reads an entity's
#' attribute details from a "entity name"_attrs.yaml or "entity name"_attrs.csv
#' file in the working directory - the details of which are incorporated into
#' the EML metadata for that entity.
#'
#' @details The \code{read_attributes_file} function reads an entity's
#' attribute details from a "entity name"_attrs.yaml or "entity name"_attrs.csv
#' file in the working directory - the details of which are incorporated into
#' the EML metadata for that entity.
#'
#' @note Recent versions of the capeml package generate attribute (and factor)
#' metadata files in yaml format; the \code{read_attributes_file} function will
#' look also for attributes files in csv format to accommodate older projects.
#'
#' @note The \code{read_attributes_file} function is intended primarily as a
#' helper to other functions in the capeml ecosystem (notably
#' \code{read_attributes} and \code{read_raster_attributes}) so is not meant to
#' be called directly (but can be).
#'
#' @param string_pointer
#' (character) The quoted name of the data entity.
#' @param entity_id
#' (character) Quoted identifier of the data object that is being described,
#' this will usually be the name or hash of the data table (or otherwise) of
#' which the attribute is associated.
#'
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom utils read.csv
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom tibble enframe
#' @importFrom dplyr pull select select_if case_when mutate bind_rows
#'
#' @return A list of a list of attributes and column classes
#'
#' @export
#'
read_attributes_file <- function(
  string_pointer,
  entity_id
  ) {

  # load attributes from yaml or csv (default to yaml)
  if (file.exists(paste0(string_pointer, "_attrs.yaml"))) {

    yaml_path <- paste0(string_pointer, "_attrs.yaml")

    # Parse YAML file with informative errors
    attrs <- tryCatch(
      yaml::yaml.load_file(yaml_path),
      error = function(e) {
        stop(paste0(
          "Failed to parse YAML file '", yaml_path, "'. ",
          e$message,
          "\nHint: Check indentation and quoting. If the first non-empty line is '|',",
          " the file is a literal block and will be parsed again as YAML text."
        ))
      }
    )

    # Parse again only if the YAML document is a block scalar (string)
    if (is.character(attrs) && length(attrs) == 1) {
      attrs_text <- attrs
      attrs <- tryCatch(
        yaml::yaml.load(attrs_text),
        error = function(e) {
          stop(paste0(
            "Failed to parse YAML literal-block content in '", yaml_path, "'. ",
            e$message,
            "\nHint: Apostrophes in single-quoted strings (e.g., don't) cause parse errors.",
            " Switch to double quotes for those values."
          ))
        }
      )
    }

    # Validate structure before flattening
    if (!is.list(attrs)) {
      stop(paste0(
        "YAML content in '", yaml_path, "' did not produce a mapping of attributes.\n",
        "Expected a named list of attribute entries; got: ", typeof(attrs)
      ))
    }
    if (is.null(names(attrs)) || any(names(attrs) == "")) {
      stop(paste0(
        "Top-level YAML mapping in '", yaml_path, "' has missing attribute names.\n",
        "Ensure each attribute (e.g., CASE_ID, Q1) is a named key."
      ))
    }

    # Robustly convert named list-of-lists to a tibble
    attrs <- tryCatch(
      dplyr::bind_rows(attrs, .id = "name") |>
        dplyr::select(-name),
      error = function(e) {
        stop(paste0(
          "Failed to convert attributes to a data frame from '", yaml_path, "'. ",
          e$message,
          "\nHint: Each attribute entry must be a mapping (named list) of fields like",
          " attributeName, attributeDefinition, columnClasses."
        ))
      }
    )

  } else if (!file.exists(paste0(string_pointer, "_attrs.yaml")) && file.exists(paste0(string_pointer, "_attrs.csv"))) {

    attrs <- utils::read.csv(paste0(string_pointer, "_attrs.csv"))

  } else {

    stop(paste0("attributes file: ", string_pointer, "_attrs.yaml ", "not found in ", getwd()))

  }

  # Ensure required columns exist
  if (!"columnClasses" %in% names(attrs)) {
    stop("Required field 'columnClasses' missing in attributes. Ensure each attribute defines 'columnClasses'.")
  }
  if (!"attributeDefinition" %in% names(attrs)) {
    attrs$attributeDefinition <- NA_character_
  }

  # column classes to vector (required by EML::set_attributes)
  classes <- attrs |>
    dplyr::pull(columnClasses)

  # copy attributeDefinition to defintion as appropriate; remove col classes
  # from attrs (req'd by set_attributes); remove empty columns (targets here
  # are max and min values, which can throw an error for data without any
  # numeric columns) empty strings to NA

  attrs[attrs == ""] <- NA

  # helper function to remove missing columns
  not_all_na <- function(x) {
    !all(is.na(x))
  }

  attrs <- attrs |>
    dplyr::mutate(
      id         = paste0(entity_id, "_", seq_len(nrow(attrs))),
      definition = NA_character_,
      definition = dplyr::case_when(
        grepl("character", columnClasses) & ((is.na(definition) | definition == "")) ~ attributeDefinition,
        TRUE ~ definition
      )
    ) |>
    dplyr::select(-columnClasses) |>
    dplyr::select_if(not_all_na)

  # return(attrs)
  return(
    list(
      attrs   = attrs,
      classes = classes
    )
  )

}
