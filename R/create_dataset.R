#' @title create EML entity of type dataset
#'
#' @description create_dataset generates a EML entity of type dataset
#'
#' @details A dataset entity, the central component of a data package, is
#'  created from objects in the user's R environment or as detailed in
#'  config.yaml. A project scope (default is LTER) indicates contact and project
#'  details specific to the research. The abstract and methods must be in
#'  markdown format - by default the package will look for these files
#'  (abstract.md, methods.md) in the project directory but files of different
#'  names or locations can be passed. Similarly for keywords, the package will
#'  look for a keywords.csv file in the project directory but a different name
#'  or location can be passed.
#'
#' @note create_dataset will look for most inputs used to construct a dataset,
#'  such as a package number, in the working environment or from the project
#'  config.yaml; these parameters are not passed directly to the function and
#'  must exist in the working environment or yaml.
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
#'
#' @return EML dataset entity is returned.
#'
#' @export
#'
create_dataset <- function(
  abstractFile = "abstract.md",
  methodsFile = "methods.md",
  keywordsFile = "keywords.csv",
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

  # retrieve title and packageIdent from config.yaml
  if (!file.exists("config.yaml")) {
    stop("config.yaml not found")
  }
  title <- yaml::yaml.load_file("config.yaml")$title
  packageIdent <- yaml::yaml.load_file("config.yaml")$packageIdent
  scope <- yaml::yaml.load_file("config.yaml")$project

  # read abstract
  tryCatch({
    abstract <- read_markdown(abstractFile)
  }, error = function(err) {
    print(paste("could not read abstract:", err))
  }) # close try catch - abstract

  # methods
  if (!exists("enhancedMethods")) {
    tryCatch({
      methods <- EML::eml$methods(methodStep = list(description = read_markdown(methodsFile)))
    }, error = function(err) {
      print(paste("could not read methods:", err))
    }) # close try catch - methods
  } else {
    methods <- enhancedMethods
  }

  # read keywords and create keywordSet
  tryCatch({
    keywords <- create_keywordSet(keywordsFile)
  }, error = function(err) {
    print(paste("could not read keywords:", err))
  }) # close try catch - keywords


  # construct base dataset with required components
  dataset <- EML::eml$dataset(
    title = title,
    creator = creators,
    pubDate = if (!is.null(publicationDate)) { publicationDate } else { as.character(Sys.Date()) },
    metadataProvider = metadataProvider,
    language = "english",
    intellectualRights = capRights,
    abstract = abstract,
    keywordSet = keywords,
    coverage = coverage,
    methods = methods,
    distribution = create_distribution(packageIdent)
  )

  # add project-specific elements
  if (grepl("lter", scope, ignore.case = TRUE)) {

    dataset$contact <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub
    dataset$project <- capProject # cap project

  } else if (grepl("gios", scope, ignore.case = TRUE)) {

    dataset$contact <- giosContact # gios contact
    dataset$publisher <- giosPublisher # gios pub

  } else if (grepl("urex", scope, ignore.case = TRUE)) {

    dataset$contact <- giosContact # gios contact
    dataset$publisher <- giosPublisher # gios pub
    dataset$project <- urexProject # urex project

  } else if (grepl("som", scope, ignore.case = TRUE)) {

    dataset$contact <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub
    dataset$project <- somProject # cap project

  } else if (grepl("ltreb", scope, ignore.case = TRUE)) {

    dataset$contact <- capContact # cap contact
    dataset$publisher <- capPublisher # cap pub
    capProject$relatedProject <- ltrebProject # ltreb as related
    dataset$project <- capProject # ltreb nested in cap

  } else {

    stop("project scope is not recognized")

  }


  # add associated party if exists
  if (exists("associatedParty")) { dataset$associatedParty <- associatedParty }


  # add literature citations if exists
  num_citations <- 0

  if (exists("citations")) {

    dataset$literatureCited <- citations
    num_citations <- length(citations$citation)

  }


  # add usage citations if exists
  num_usages <- 0

  if (exists("usages")) {

    dataset$usageCitation <- usages
    num_usages <- length(usages)

  }


  # generate summary
  message(
    paste0(
      "created EML dataset:\n",
      " package: ", packageIdent, "\n",
      " title: ", title, "\n",
      " scope: ", scope, "\n",
      " associated party: ", exists("associatedParty"), "\n",
      " literature citations: ", num_citations, "\n",
      " usage citations: ", num_usages, "\n"
    )
  )


  # return
  return(dataset)

}
