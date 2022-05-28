#' @title create EML entity of type dataset
#'
#' @description create_dataset generates a EML entity of type dataset
#'
#' @details A dataset entity, the central component of a data package, is
#' created from objects in the user's R environment or as detailed in
#' config.yaml. A project name (default is LTER) indicates contact and project
#' details specific to the research. The abstract and methods must be in
#' markdown format - by default the package will look for these files
#' (abstract.md, methods.md) in the project directory but files of different
#' names or locations can be passed. Similarly for keywords, the package will
#' look for a keywords.csv file in the project directory but a different name
#' or location can be passed.
#'
#' @note create_dataset will look for most inputs used to construct a dataset,
#' such as a package number, in the working environment or from the project
#' config.yaml; these parameters are not passed directly to the function and
#' must exist in the working environment or configuration file.
#'
#' @param abstractFile
#' (character) Quoted name and path of abstract (in markdown format)
#' @param methodsFile
#' (character) Quoted name and path of methods (in markdown format)
#' @param keywordsFile
#' (character) Quoted name and path of keywords (in csv format)
#' @param publicationDate
#' (character) Quoted ISO date - defaults to today's date
#'
#' @import EML
#' @importFrom yaml yaml.load_file
#' @importFrom stringr str_extract
#'
#' @return EML dataset entity is returned.
#'
#' @export
#'
create_dataset <- function(
  abstractFile    = "abstract.md",
  methodsFile     = "methods.md",
  keywordsFile    = "keywords.csv",
  publicationDate = NULL
  ) {

  # confirm required components exist in R environment

  if (!exists("creators")) {
    stop("missing creator")
  }

  if (!exists("metadataProvider")) {
    stop("missing metadata provider")
  }

  if (!exists("coverage")) {
    stop("missing coverage")
  }

  if (!file.exists("config.yaml")) {
    stop("config.yaml not found")
  }


  # retrieve dataset details from config.yaml

  configurations <- yaml::yaml.load_file("config.yaml")
  project        <- configurations[["project"]]
  maintenance    <- configurations[["maintenance"]]


  # package scope

  if (exists("packageIdent", configurations)) {

    this_scope <- stringr::str_extract(
      string  = configurations[["packageIdent"]],
      pattern = "^[^\\.]*"
    )

  } else if (exists("scope", configurations)) {

    this_scope <- configurations[["scope"]]

  } else {

    stop("could not resolve package scope")

  }


  # package identifier (number)

  if (exists("packageNum", configurations)) {

    this_identifier <- stringr::str_extract(
      string  = configurations[["packageNum"]],
      pattern = "[0-9]+"
    )

  } else if (exists("identifier", configurations)) {

    this_identifier <- configurations[["identifier"]]

  } else {

    stop("could not resolve package identifier (number)")

  }


  this_identifier <- as.integer(this_identifier)


  # package version

  this_version <- capeml::get_next_version(
    provided_scope      = this_scope,
    provided_identifier = this_identifier
  )


  # package name (scope + identifier +  version)

  package_name <- paste(this_scope, this_identifier, this_version, sep = ".")


  # abstract

  tryCatch({

    abstract <- capeml::read_markdown(abstractFile)

  }, error = function(err) {

    stop("could not read abstract.md: ", err)

  }) # close try catch - abstract


  # methods

  if (!exists("enhancedMethods")) {

    tryCatch({

      methods <- EML::eml$methods(methodStep = list(description = capeml::read_markdown(methodsFile)))

    }, error = function(err) {

      stop("could not read methods.md: ", err)

    }) # close try catch - methods

  } else {

    methods <- enhancedMethods

  }


  # keywords

  tryCatch({

    keywords <- capeml::create_keywordSet(keywordsFile)

  }, error = function(err) {

    stop("could not read keywords.csv: ", err)

  }) # close try catch - keywords


  # maintenance

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


  # construct base dataset with required components

  dataset <- EML::eml$dataset(
    title              = configurations[["title"]],
    creator            = creators,
    pubDate            = if (!is.null(publicationDate)) { publicationDate } else { as.character(Sys.Date()) },
    metadataProvider   = metadataProvider,
    language           = "english",
    intellectualRights = capRights,
    abstract           = abstract,
    keywordSet         = keywords,
    coverage           = coverage,
    maintenance        = dataset_maintenance,
    methods            = methods,
    distribution       = create_distribution(package_name)
  )


  # add project-specific elements

  if (grepl("lter", project, ignore.case = TRUE)) {

    dataset$contact   <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub
    dataset$project   <- configure_caplter_project() # cap project

  } else if (grepl("gios", project, ignore.case = TRUE)) {

    dataset$contact   <- giosContact # gios contact
    dataset$publisher <- giosPublisher # gios pub

  } else if (grepl("urex", project, ignore.case = TRUE)) {

    dataset$contact   <- giosContact # gios contact
    dataset$publisher <- giosPublisher # gios pub
    dataset$project   <- configure_urex_project()  # urex project

  } else if (grepl("som", project, ignore.case = TRUE)) {

    dataset$contact   <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub
    dataset$project   <- configure_som_project()  # SOM project

  } else if (grepl("ltreb", project, ignore.case = TRUE)) {

    dataset$contact   <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub

    # nest LTREB under CAP

    cap_project                <- configure_caplter_project()
    cap_project$relatedProject <- configure_ltreb_project()
    dataset$project            <- cap_project

  } else {

    project <- "none"

  }


  # add dataTable(s) if exist(s)

  if (length(ls(envir = .GlobalEnv, pattern = "_DT")) > 0) {

    listOfDataTables  <- lapply(ls(envir = .GlobalEnv, pattern = "_DT"), function(DT) { get(DT, envir = .GlobalEnv) } )
    dataset$dataTable <- listOfDataTables

  }


  # add otherEntity(ies) if exist(s)

  if (length(ls(envir = .GlobalEnv, pattern = "_OE")) > 0) {

    list_of_otherEntities <- lapply(ls(envir = .GlobalEnv, pattern = "_OE"), function(OE) { get(OE, envir = .GlobalEnv) } )
    dataset$otherEntity   <- list_of_otherEntities

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

  if (exists("associatedParty")) { dataset$associatedParty <- associatedParty }


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
      " title: ",                title, "\n",
      " project: ",              project, "\n",
      " maintenance: ",          maintenance, "\n",
      " dataTables: ",           paste0(c(ls(envir = .GlobalEnv, pattern = "_DT")), collapse = ", "), "\n",
      " otherEntities: ",        paste0(c(ls(envir = .GlobalEnv, pattern = "_OE")), collapse = ", "), "\n",
      " spatialVectors: ",       paste0(c(ls(envir = .GlobalEnv, pattern = "_SV")), collapse = ", "), "\n",
      " spatialRasters: ",       paste0(c(ls(envir = .GlobalEnv, pattern = "_SR")), collapse = ", "), "\n",
      " associated party: ",     exists("associatedParty"), "\n",
      " literature citations: ", num_citations, "\n",
      " usage citations: ",      num_usages, "\n"
    )
  )


  # return

  return(dataset)

}
