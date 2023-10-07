#' @title write a template yaml file for adding personnel (people) metadata
#'
#' @description Write a template yaml file (people.yaml) for adding pesonnel
#' (people) metadata
#'
#' @param path
#'  (character) Path to where the yaml file will be written. Defaults to the
#'  current directory.
#' @param overwrite
#'  (logical) Indicates to overwrite an existing file if one exists.
#'
#' @importFrom yaml write_yaml
#'
#' @return A people.yaml file in the prescribed directory for adding personnel
#' metadata; initially populated with inputs for a single <creator>,
#' <metadataProvider>, and <associatedParty>
#'
#' @export
#'
write_people_template <- function(
  path      = ".",
  overwrite = FALSE
  ) {

  target_path_file <- paste0(path, "/people.yaml")

  # stop if file exists

  if (file.exists(target_path_file) && overwrite == FALSE) {
    stop(
      paste0(target_path_file, " already exists (use overwrite)")
    )
  }

  list(
    list(
      "last_name"   = NULL,
      "first_name"  = NULL,
      "middle_name" = NULL,
      "role_type"   = "creator",
      "email"       = NULL,
      "orcid"       = NULL,
      "data_source" = NULL
      ),
    list(
      "last_name"   = NULL,
      "first_name"  = NULL,
      "middle_name" = NULL,
      "role_type"   = "metadataProvider",
      "email"       = NULL,
      "orcid"       = NULL,
      "data_source" = NULL
      ),
    list(
      "last_name"    = NULL,
      "first_name"   = NULL,
      "middle_name"  = NULL,
      "role_type"    = "associatedParty",
      "project_role" = NULL,
      "email"        = NULL,
      "orcid"        = NULL,
      "data_source"  = NULL
    )
    ) |>
  yaml::write_yaml(file = target_path_file)

}
