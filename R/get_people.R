#' @title translate personnel metadata to eml
#'
#' @description \code{get_people} harvests metadata pertaining to deataset
#' personnel (people) documented in a `people.yaml` file and translates those
#' metadata to a lists appropriate for inclusion in a EML <dataset> as
#' `EML::eml$creator`, `EML::eml$metadataProvider`, and
#' `EML::eml$associatedParty` as relevant.
#'
#' @import EML
#' @importFrom yaml yaml.load_file
#' @importFrom purrr map keep
#'
#' @return lists of data personnel appropriate for inclusion in a EML <dataset>
#' as `EML::eml$creator`, `EML::eml$metadataProvider`, and
#' `EML::eml$associatedParty` as relevant
#'
#' @export
#'
get_people <- function() {

  from_load <- yaml::yaml.load_file("people.yaml")

  creators <- from_load |> 
    purrr::keep(\(x) grepl("creator", x[["role_type"]], ignore.case = TRUE)) |> 
    purrr::map(\(x) create_role(x))

  metadata_providers <- from_load |> 
    purrr::keep(\(x) grepl("metadata", x[["role_type"]], ignore.case = TRUE)) |> 
    purrr::map(\(x) create_role(x))

  associated_parties <- from_load |> 
    purrr::keep(\(x) grepl("associate", x[["role_type"]], ignore.case = TRUE)) |> 
    purrr::map(\(x) create_role(x))

  return(
    list(
      creators           = creators,
      metadata_providers = metadata_providers,
      associated_parties = associated_parties
    )
  )

}

#' @description \code{validate_input} is a helper called primarily by the
#' \code{create_role} function to test and apply appropriate formatting of
#' provided metadata to function calls.

validate_input <- function(object) {

  if (
    length(object) == 1 && (!is.null(object) || !is.na(object) || !is.nan(object))
    ) {

    return(TRUE)

  } else {

    return(FALSE)

  }

}

#' @description \code{create_role} is a helper function that generates a list
#' EML personnel entities (e.g., creator, metadataProvider). \code{create_role}
#' builds the list of people from results returned from a query of a personnel
#' data source using the \code{get_person_attributes} function or from person
#' details provided in a people.yaml metadata file. In the case of querying a
#' data source, person attributes can be queried by a combination of first and
#' last name (both required but partial matching possible).

create_role <- function(
  person_metadata,
  project_role = "former associate of project"
  ) {

  if (validate_input(person_metadata[["data_source"]])) {

    validate_attribute <- function(person_attribute) {

      validated_attribute <- ifelse(
        test = is.na(person_from_file[[person_attribute]]) || is.null(person_from_file[[person_attribute]]),
        yes  = NA,
        no   = person_from_file[[person_attribute]]
      )

      return(validated_attribute)

    }

    person_from_file <- get_person_attributes(
      this_last_name   = person_metadata[["last_name"]],
      this_first_name  = person_metadata[["first_name"]],
      this_data_source = person_metadata[["data_source"]]
    )

    person_metadata[["last_name"]]    <- person_from_file[["last_name"]]
    person_metadata[["first_name"]]   <- validate_attribute("first_name")
    person_metadata[["middle_name"]]  <- validate_attribute("middle_name")
    person_metadata[["organization"]] <- validate_attribute("organization")
    person_metadata[["email"]]        <- validate_attribute("email")
    person_metadata[["orcid"]]        <- validate_attribute("orcid")

  }

  # use middle when present
  if (validate_input(person_metadata[["middle_name"]])) {

    this_first_name <- c(
      person_metadata[["first_name"]],
      person_metadata[["middle_name"]]
    )

  } else {

    this_first_name <- person_metadata[["first_name"]]

  }

  # establish project role if relevant
  if (validate_input(person_metadata[["project_role"]])) {

    project_role <- person_metadata[["project_role"]]

  }

  # establish base person
  this_role_type <- person_metadata[["role_type"]] 

  if (length(this_role_type) == 1 && grepl("meta", this_role_type)) {

    eml_person <- EML::eml$metadataProvider()

  } else if (length(this_role_type) == 1 && grepl("assoc", this_role_type)) {

    eml_person      <- EML::eml$associatedParty()
    eml_person$role <- person_metadata[["project_role"]]

  } else if (length(this_role_type) == 1 && grepl("person", this_role_type)) {

    eml_person <- EML::eml$personnel()

  } else {

    eml_person <- EML::eml$creator()

  }

  # # add requisite details
  eml_person$individualName$givenName <- this_first_name
  eml_person$individualName$surName   <- person_metadata[["last_name"]]
  eml_person$electronicMailAddress    <- person_metadata[["email"]]
  eml_person$organizationName         <- person_metadata[["organization"]]


  # add orcid if exists
  if (validate_input(person_metadata[["orcid"]])) {

    eml_person$userId$userId    <- person_metadata[["orcid"]]
    eml_person$userId$directory <- "https://orcid.org"

  }

  return(eml_person)

}


#' @description \code{get_person_attributes} is a helper function that
#' generates a data frame of person attributes queried from a data source. The
#' function is intended primarily as a helper function for the
#' \code{create_role} function that generates a list suitable for a EML entity
#' of type person. Person attributes can be queried by a combination of first
#' and last name (both required but partial matching possible).

get_person_attributes <- function(
  this_last_name,
  this_first_name = NULL,
  this_data_source
  ) {

  # require data source
  if (missing(this_data_source)) {

    stop("missing data source")

  }

  # require (at least) last name
  if (missing(this_last_name)) {

    stop("must provide a last name")

  }

  authors <- suppressMessages(readr::read_csv(this_data_source))

  author <- authors |>
    dplyr::filter(
      grepl(this_last_name, last_name, ignore.case = TRUE)
    )

  if (!is.null(this_first_name)) {

    author <- author |>
      dplyr::filter(
        grepl(this_first_name, first_name, ignore.case = TRUE)
      )

  }

  if (nrow(author) > 1) {

    message("more than one record identified")
    author <- NULL

  } else if (nrow(author) < 1) {

    message("person not found")
    author <- NULL

  } else {

    author

  }

  return(author)

}
