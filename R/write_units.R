#' @title Create template yaml file(s) for supplying attribute unit metadata
#'
#' @description \code{write_units} creates template yaml file(s) for supplying
#' attribute QUDT and custom unit metadata for a data object that resides in
#' the R environment.
#'
#' @details \code{write_units} generates zero to two yaml template files based
#' on the reference of attribute units associated with a data entity. A
#' `annotations.yaml` template is generated if attribute units are in the QUDT
#' unit dictionary. A `custom_units.yaml` template is generated for custom
#' (i.e., not in the EML standard units or QUDT dictionaries). Both templates
#' will be created if both QUDT and custom units are identified, and neither
#' template will be written if the data entity does not have units or if all of
#' the units are in the EML standard dictionary (because further documentation
#' is not required). Unit metadata will be appended to templates if they
#' already exist in the working directory.
#'
#' @note \code{write_units} evalutes a data object's attributes metadata file
#' (e.g., `my_table_attrs.yaml`). As such, this file must exist in the working
#' directory.
#'
#' @param entity_name
#'  (character) Quoted name of the data object in the R environment
#' @param entity_id
#'  (character) A string that uniquely identifies the object in the R
#'  environment (e.g., the hash of a table)
#'
#' @import yaml
#' @importFrom dplyr inner_join mutate
#' @importFrom purrr map list_rbind list_transpose
#'
#' @return `annotations.yaml`, `custom_units.yaml`, both, or neither depending
#' on the unit reference (QUDT, custom, EML standard unit library) used to
#' characterize attribute units for a data entity.
#'
#' @examples
#' \dontrun{
#'
#'  # create units metadata file(s) from the algae_attrs.yaml
#'  # file in the working directory
#'
#'  algae <- read.csv("algae.csv")
#'
#'  capeml::write_attributes(
#'    entity_name = "algae",
#'    entity_id   = tools::md5sum("algae.csv")
#'  )
#'
#' }
#'
#' @export
#'
write_units <- function(
  entity_name,
  entity_id
) {

  attributes_table <- capeml::read_attributes(
    entity_name = entity_name,
    entity_id   = entity_id
  )[["table"]]

  attributes_units_unique <- unique(attributes_table[!is.na(attributes_table$unit), ][["unit"]])
  attributes_units_types  <- purrr::map(.x = attributes_units_unique, ~ capeml::get_unit_type(this_unit = .x))

  if (!all(sapply(attributes_units_types, is.null))) {

    qudt_and_custom <- attributes_units_types |> 
      purrr::list_rbind() |> 
      dplyr::inner_join(
        y  = attributes_table[, c("attributeName", "id", "unit")],
        by = c("name" = "unit")
      )

    # type: QUDT

    if (nrow(qudt_and_custom[grepl("qudt", qudt_and_custom$type, ignore.case = TRUE), ]) > 0) {

      qudt_units <- qudt_and_custom[grepl("qudt", qudt_and_custom$type, ignore.case = TRUE), ]
      qudt_units$id_name <- paste0(qudt_units$id, "_", qudt_units$name)

      qudt_annotations <- split(
        x = qudt_units,
        f = qudt_units$id_name
      ) |>
        {\(row) purrr::map(.x = row, ~ 
          list(
            name          = .x$name,
            valueLabel    = .x$label,
            valueURI      = .x$unit,
            references    = .x$id,
            propertyLabel = "has unit",
            propertyURI   = "http://qudt.org/schema/qudt/hasUnit"
          )
        )}()


      if (file.exists("annotations.yaml")) {

        existing_annotations <- yaml::yaml.load_file("annotations.yaml")

        c(existing_annotations, qudt_annotations) |> 
          yaml::write_yaml(
            file         = "annotations.yaml",
            column.major = FALSE
          )

      } else {

        yaml::write_yaml(
          x            = qudt_annotations,
          file         = "annotations.yaml",
          column.major = FALSE
        )

      }

    }


    # type: custom

    if (nrow(qudt_and_custom[qudt_and_custom$type == "custom", ]) > 0) {

      new_custom_units <- qudt_and_custom[qudt_and_custom$type == "custom", ]["name"] |> 
        dplyr::mutate(description = "") |>
        as.list() |> 
        purrr::list_transpose(simplify = FALSE)

      message("new_custom_units: ", qudt_and_custom[qudt_and_custom$type == "custom", ]["name"])

      if (file.exists("custom_units.yaml")) {

        existing_custom_units <- yaml::yaml.load_file("custom_units.yaml")

        c(existing_custom_units, new_custom_units) |> 
          unique() |> 
          yaml::write_yaml(
            file         = "custom_units.yaml",
            column.major = FALSE
          )

      } else {

        yaml::write_yaml(
          x            = new_custom_units,
          file         = "custom_units.yaml",
          column.major = FALSE
        )

      }

    }

  } else {

    message(entity_name, ": neither QUDT or custom units were detected")
    return(NULL)

  }

}
