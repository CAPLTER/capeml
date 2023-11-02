#' @title create a EML entity of type dataset
#'
#' @description \code{create_dataset} generates a EML entity of type dataset.
#'
#' @details A dataset entity, the central component of a data package, is
#' created from objects in the user's R environment and supporting yaml files.
#' A project (default is LTER) indicates contact and project details specific
#' to the research. The abstract and methods must be in markdown format -
#' package will look for these files (abstract.md, methods.md) in the project
#' directory. Similarly for keywords, the package will look for a keywords.csv
#' file in the project directory.
#'
#' @note \code{create_dataset} will look for most inputs used to construct a
#' dataset, such as a package number, in the working environment or from the
#' project config.yaml; these parameters are not passed directly to the
#' function and must exist in the working environment or configuration file(s).
#'
#' @param publication_date
#' (character) Quoted ISO date - defaults to \code{Sys.Date()}
#'
#' @import EML
#' @importFrom yaml yaml.load_file
#' @importFrom purrr map map_chr every
#'
#' @return EML entity of type dataset entity is returned.
#'
#' @export
#'
create_dataset <- function(
  publication_date = NULL
  ) {

  # confirm required components exist in R environment

  if (!exists("coverage")) {
    stop("missing coverage")
  }


  # retrieve dataset details from config.yaml

  configurations <- capeml::read_package_configuration()


  # package version

  this_version <- capeml::get_next_version(
    provided_scope      = configurations$scope,
    provided_identifier = configurations$identifier,
    display_message     = TRUE
  )


  # package name (scope + identifier +  version)

  package_name <- paste(
    configurations$scope,
    configurations$identifier,
    this_version,
    sep = "."
  )

  # abstract

  tryCatch({

    abstract <- capeml::read_markdown("abstract.md")

  }, error = function(err) {

    stop("could not read abstract.md: ", err)

  }) # close try catch - abstract


  # methods

  if (!exists("rich_methods")) {

    tryCatch({

      methods <- EML::eml$methods(
        methodStep = list(
          description = capeml::read_markdown("methods.md")
        )
      )

    }, error = function(err) {

      stop("could not read methods.md: ", err)

    }) # close try catch - methods

  } else {

    methods <- rich_methods

  }


  # keywords

  tryCatch({

    keywords <- capeml::create_keywordSet("keywords.csv")

  }, error = function(err) {

    stop("could not read keywords.csv: ", err)

  }) # close try catch - keywords


  # maintenance

  maintenance <- configurations$maintenance

  if (maintenance == "regular") {

    dataset_maintenance <- EML::eml$maintenance(
      description                = "it is expected that dataset will receive regular updates (approximately annually or as needed)",
      maintenanceUpdateFrequency = "asNeeded"
    )

  } else if (maintenance == "none") {

    dataset_maintenance <- EML::eml$maintenance(
      description                = "this dataset is complete and or updates are not anticipated",
      maintenanceUpdateFrequency = "notPlanned"
    )

  } else {

    dataset_maintenance <- NULL
    maintenance         <- "not specified (none, regular)"

  }


  # distribution

  cap_url <- list(
    url        = "https://cap.lternet.edu",
    `function` = "information"
  )

  cap_online       <- EML::eml$online(url = cap_url)
  cap_distribution <- EML::eml$distribution(online = cap_online)


  # construct base dataset with required components

  dataset <- EML::eml$dataset(
    title              = configurations$title,
    pubDate            = if (!is.null(publication_date)) { publication_date } else { as.character(Sys.Date()) },
    language           = "english",
    intellectualRights = capRights,
    abstract           = abstract,
    keywordSet         = keywords,
    coverage           = coverage,
    maintenance        = dataset_maintenance,
    methods            = methods,
    distribution       = cap_distribution
  )


  # personnel

  people <- capeml::get_people()

  ## add to dataset

  dataset$creator          <- people[["creators"]]
  dataset$metadataProvider <- people[["metadata_providers"]]
  dataset$associatedParty  <- people[["associated_parties"]]

  ## summarize people

  list_of_creators <- purrr::map(people[["creators"]], "individualName") |> 
  purrr::map_chr("surName")

  if (
    purrr::map(people[["metadata_providers"]], "individualName") |> 
    purrr::map("surName") |> 
    purrr::every(is.null)
    ) {

    list_of_metadata_providers <- NULL

  } else {

    list_of_metadata_providers <- purrr::map(people[["metadata_providers"]], "individualName") |> 
    purrr::map_chr("surName")

  }

  if (
    purrr::map(people[["associated_parties"]], "individualName") |> 
    purrr::map("surName") |> 
    purrr::every(is.null)
    ) {

    list_of_associated_parties <- NULL

  } else {

    list_of_associated_parties <- purrr::map(people[["associated_parties"]], "individualName") |> 
    purrr::map_chr("surName")

  }


  # add project-specific elements

  project <- configurations$project

  if (grepl("lter", project, ignore.case = TRUE)) {

    dataset$contact   <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub
    dataset$project   <- capeml::configure_caplter_project() # cap project

  } else if (grepl("gios", project, ignore.case = TRUE)) {

    dataset$contact   <- giosContact # gios contact
    dataset$publisher <- giosPublisher # gios pub

  } else if (grepl("urex", project, ignore.case = TRUE)) {

    dataset$contact   <- giosContact # gios contact
    dataset$publisher <- giosPublisher # gios pub
    dataset$project   <- capeml::configure_urex_project()  # urex project

  } else if (grepl("som", project, ignore.case = TRUE)) {

    dataset$contact   <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub
    dataset$project   <- capeml::configure_som_project()  # SOM project

  } else if (grepl("ltreb", project, ignore.case = TRUE)) {

    dataset$contact   <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub

    # nest LTREB under CAP

    cap_project                <- capeml::configure_caplter_project()
    cap_project$relatedProject <- capeml::configure_ltreb_project()
    dataset$project            <- cap_project

  } else {

    project <- "none"

  }


  # load data objects as outlined in data_objects yaml

  if (file.exists("data_objects.yaml")) {

    data_objects_load_file <- yaml::yaml.load_file("data_objects.yaml")
    data_entities          <- purrr::map(.x = data_objects_load_file, ~ create_entities_from_yaml(entity_configuration = .x))
    data_entities_yaml     <- TRUE

  } else {

    data_entities_yaml     <- FALSE

  }

  # data tables (DT)

  ## tables from yaml

  if (data_entities_yaml == TRUE) {

    table_entities <- purrr::map(data_entities, ~ .x[["type"]] == "table")
    table_entities <- data_entities[unlist(table_entities, use.names = FALSE)]
    table_names    <- names(table_entities)
    table_entities <- purrr::map(table_entities, ~ .x[["entity"]])

  } else {

    table_entities <- NULL
    table_names    <- NULL

  }

  ## tables from _DT

  if (length(ls(envir = .GlobalEnv, pattern = "_DT")) > 0) {

    listOfDataTables <- lapply(ls(envir = .GlobalEnv, pattern = "_DT"), function(DT) { get(DT, envir = .GlobalEnv) } )
    table_names      <- c(table_names, c(ls(envir = .GlobalEnv, pattern = "_DT")))
    table_entities   <- c(table_entities, listOfDataTables)

  }

  ## dataset tables

  if (length(table_entities) > 0) { 

    dataset$dataTable <- unname(table_entities)

  }


  # other entities (OE)

  ## other entities from yaml

  if (data_entities_yaml == TRUE) {

    other_entities     <- purrr::map(data_entities, ~ .x[["type"]] == "other")
    other_entities     <- data_entities[unlist(other_entities, use.names = FALSE)]
    other_entity_names <- names(other_entities)
    other_entities     <- purrr::map(other_entities, ~ .x[["entity"]])

  } else {

    other_entities     <- NULL
    other_entity_names <- NULL

  }

  ## other entities from _OE

  if (length(ls(envir = .GlobalEnv, pattern = "_OE")) > 0) {

    list_of_otherEntities <- lapply(ls(envir = .GlobalEnv, pattern = "_OE"), function(OE) { get(OE, envir = .GlobalEnv) } )
    other_entity_names    <- c(other_entity_names, c(ls(envir = .GlobalEnv, pattern = "_OE")))
    other_entities        <- c(other_entities, list_of_otherEntities)

  }

  ## dataset other entities

  if (length(other_entities) > 0) { 

    dataset$otherEntity   <- unname(other_entities)

  }


  # add spatialVector(s) if exist(s)

  if (length(ls(envir = .GlobalEnv, pattern = "_SV")) > 0) {

    list_of_spatialVectors <- lapply(ls(envir = .GlobalEnv, pattern = "_SV"), function(SV) { get(SV, envir = .GlobalEnv) } )
    dataset$spatialVector  <- list_of_spatialVectors

  }


  # add spatialRaster(s) if exist(s)

  if (length(ls(envir = .GlobalEnv, pattern = "_SR")) > 0) {

    list_of_spatialRasters <- lapply(ls(envir = .GlobalEnv, pattern = "_SR"), function(SR) { get(SR, envir = .GlobalEnv) } )
    dataset$spatialRaster  <- list_of_spatialRasters

  }


  # add associated party if exists

  # if (exists("associatedParty")) { dataset$associatedParty <- associatedParty }


  # add literature citations if exists

  num_citations <- 0

  if (exists("citations")) {

    dataset$literatureCited <- citations
    num_citations           <- length(citations$citation)

  }


  # add usage citations if exists
  num_usages <- 0

  if (exists("usages")) {

    dataset$usageCitation <- usages
    num_usages            <- length(usages)

  }


  # generate summary

  message(
    paste0(
      "created EML dataset:\n",
      " package: ",              package_name, "\n",
      " title: ",                configurations$title, "\n",
      " project: ",              project, "\n",
      " maintenance: ",          maintenance, "\n",
      " dataTables: ",           paste0(c(table_names), collapse = ", "), "\n",
      " otherEntities: ",        paste0(c(other_entity_names), collapse = ", "), "\n",
      " spatialVectors: ",       paste0(c(ls(envir = .GlobalEnv, pattern = "_SV")), collapse = ", "), "\n",
      " spatialRasters: ",       paste0(c(ls(envir = .GlobalEnv, pattern = "_SR")), collapse = ", "), "\n",
      " creators: ",             paste0(list_of_creators, collapse = ", "), "\n",
      " metadata providers: ",   paste0(list_of_metadata_providers, collapse = ", "), "\n",
      " associated parties: ",   paste0(list_of_associated_parties, collapse = ", "), "\n",
      " literature citations: ", num_citations, "\n",
      " usage citations: ",      num_usages, "\n"
    )
  )

  # return

  return(dataset)

}
