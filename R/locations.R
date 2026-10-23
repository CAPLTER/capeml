#' @title Resolve data and metadata locations for a project
#'
#' @description \code{get_locations} reads the optional \code{data_path} and
#' \code{metadata_path} keys from config.yaml in the working directory.
#'
#' @details Both keys are optional, as is config.yaml itself, so that projects
#' in which all data and metadata reside in the working directory work without
#' any configuration. \code{data_path} may be one or more directories that are
#' searched (recursively, if needed) for data files; \code{metadata_path} is a
#' single directory to which attribute and factor templates are written and
#' from which they are read. Relative paths are interpreted relative to the
#' working directory and a leading tilde is expanded.
#'
#' @examples
#' \dontrun{
#'
#'  # config.yaml
#'  # data_path:
#'  #   - /scratch/project/raw
#'  #   - /data/shared/lter
#'  # metadata_path: /scratch/project/metadata
#'
#'  capeml::get_locations()
#'
#' }
#'
#' @importFrom yaml yaml.load_file
#'
#' @return A list with elements \code{data_path} (character vector, NULL if
#' not configured) and \code{metadata_path} (character, "." if not
#' configured).
#'
#' @export
#'
get_locations <- function() {

  locations <- list(
    data_path     = NULL,
    metadata_path = "."
  )

  if (!file.exists("config.yaml")) {
    return(locations)
  }

  configurations <- yaml::yaml.load_file("config.yaml")

  if (!is.null(configurations[["data_path"]])) {

    data_path <- path.expand(unlist(configurations[["data_path"]], use.names = FALSE))
    missing_roots <- data_path[!dir.exists(data_path)]

    if (length(missing_roots) > 0) {
      stop("data_path in config.yaml not found: ", paste(missing_roots, collapse = ", "))
    }

    locations[["data_path"]] <- data_path

  }

  if (!is.null(configurations[["metadata_path"]])) {

    if (length(configurations[["metadata_path"]]) != 1) {
      stop("metadata_path in config.yaml must be a single directory")
    }

    locations[["metadata_path"]] <- path.expand(configurations[["metadata_path"]])

  }

  return(locations)

}


#' @title Locate a data file or directory
#'
#' @description \code{find_data_file} resolves the location of a data file or
#' directory that may reside in the working directory or in any of the
#' directories identified by \code{data_path} in config.yaml.
#'
#' @details The search proceeds from least to most expensive: (1) the path as
#' given (absolute or relative to the working directory); (2) the path relative
#' to each \code{data_path} directory; (3) a recursive search of each
#' \code{data_path} directory for a file or directory whose path ends in
#' \code{file}. The recursive search is a fallback only because listing large
#' directory trees can be slow, particularly on networked or parallel file
#' systems. An error is raised if the target is not found or if more than one
#' candidate is found.
#'
#' @param file
#'  (character) The quoted name (and, optionally, partial path) of the data
#'  file or directory.
#' @param data_path
#'  (character) One or more directories to search. Defaults to
#'  \code{data_path} in config.yaml.
#'
#' @examples
#' \dontrun{
#'
#'  algae <- readr::read_csv(capeml::find_data_file("algae.csv"))
#'
#' }
#'
#' @return The path to the data file or directory.
#'
#' @export
#'
find_data_file <- function(
  file,
  data_path = NULL
  ) {

  if (file.exists(file)) {
    return(file)
  }

  if (is.null(data_path)) {
    data_path <- get_locations()[["data_path"]]
  }

  roots <- sub("/$", "", path.expand(data_path))

  # path relative to each data root

  candidates <- file.path(roots, file)
  candidates <- candidates[file.exists(candidates)]

  # recursive search of each data root

  if (length(candidates) == 0) {

    candidates <- unlist(
      purrr::map(
        .x = roots,
        .f = ~ list.files(
          path         = .x,
          recursive    = TRUE,
          full.names   = TRUE,
          include.dirs = TRUE
        )
      ),
      use.names = FALSE
    )

    candidates <- candidates[endsWith(candidates, paste0("/", sub("^\\./", "", file)))]

  }

  if (length(candidates) == 0) {

    stop(
      "could not find '", file, "' in the working directory",
      if (length(roots) > 0) paste0(" or data_path: ", paste(roots, collapse = ", "))
    )

  }

  if (length(candidates) > 1) {

    stop(
      "multiple candidates for '", file, "', provide a more specific path:\n",
      paste(candidates, collapse = "\n")
    )

  }

  return(candidates)

}


#' @title Locate an attribute or factor metadata template
#'
#' @description \code{find_template} locates an entity's attribute or factor
#' metadata template (e.g., my_table_attrs.yaml) in the \code{metadata_path}
#' directory or the working directory.
#'
#' @details Within a directory, a yaml template takes precedence over a csv
#' template. An error is raised if a template for the entity is found in more
#' than one directory so that a stale copy is never used silently.
#'
#' @param name
#'  (character) The quoted name of the data entity.
#' @param type
#'  (character) The template type: "attrs" or "factors".
#' @param metadata_path
#'  (character) Directory of metadata templates. Defaults to
#'  \code{metadata_path} in config.yaml, else the working directory.
#' @param also
#'  (character) Additional directories to search (e.g., the directory of a
#'  data file for templates co-located with the data).
#' @param extensions
#'  (character) Template file extensions to consider, in order of precedence.
#' @param must_exist
#'  (logical) Whether to raise an error if a template is not found.
#'
#' @return The path to the template, or NULL if a template is not found and
#' \code{must_exist} is FALSE.
#'
#' @export
#'
find_template <- function(
  name,
  type          = c("attrs", "factors"),
  metadata_path = NULL,
  also          = NULL,
  extensions    = c("yaml", "csv"),
  must_exist    = FALSE
  ) {

  type <- match.arg(type)

  if (is.null(metadata_path)) {
    metadata_path <- get_locations()[["metadata_path"]]
  }

  directories <- path.expand(c(metadata_path, ".", also))
  directories <- directories[!duplicated(normalizePath(directories, mustWork = FALSE))]

  found <- purrr::map(
    .x = directories,
    .f = function(directory) {
      templates <- file.path(directory, paste0(name, "_", type, ".", extensions))
      templates[file.exists(templates)][1]
    }
  ) |>
    unlist(use.names = FALSE) |>
    stats::na.omit()

  if (length(found) > 1) {

    stop(
      "found ", type, " templates for '", name, "' in more than one location, remove all but one:\n",
      paste(found, collapse = "\n")
    )

  }

  if (length(found) == 0) {

    if (must_exist) {

      stop(
        if (type == "attrs") "attributes" else type,
        " file not found for '", name, "' in: ",
        paste(directories, collapse = ", ")
      )

    }

    return(NULL)

  }

  return(as.character(found))

}


#' @title Path to which a metadata template is written
#'
#' @description \code{template_target} returns the yaml path in
#' \code{metadata_path} to which a template is written, creating the directory
#' if needed. An error is raised if a template for the entity already exists
#' in another location as that would yield duplicate templates.
#'
#' @noRd
#'
template_target <- function(
  name,
  type,
  metadata_path = NULL
  ) {

  if (is.null(metadata_path)) {
    metadata_path <- get_locations()[["metadata_path"]]
  }

  metadata_path <- path.expand(metadata_path)
  target        <- file.path(metadata_path, paste0(name, "_", type, ".yaml"))
  existing      <- find_template(name, type, metadata_path)

  if (
    !is.null(existing) &&
      normalizePath(dirname(existing)) != normalizePath(metadata_path, mustWork = FALSE)
    ) {

    stop(
      type, " template for '", name, "' already exists at ", existing,
      "; remove it or set metadata_path to its directory"
    )

  }

  if (!dir.exists(metadata_path)) {
    dir.create(metadata_path, recursive = TRUE)
  }

  return(target)

}
