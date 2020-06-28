#' @title create_dataset
#'
#' @description create_dataset generates a EML entity of type dataset
#'
#' @details A dataset entity, the central component of a data package, is
#'   created from objects in the user's R environment. A project scope (default
#'   is LTER) indicates contact and project details specific to the research.
#'   The abstract and methods must be in markdown format - by default the
#'   package will look for these files (abstract.md, methods.md) in the project
#'   directory but files of different names or locations can be passed.
#'   Similarly for keywords, the package will look for a keywords.csv file in
#'   the project directory but a different name or location can be passed.
#'
#' @note create_dataset will look for most inputs used to construct a dataset,
#'   such as a project id, in the working environment; these parameters are not
#'   passed directly to the function and must exist in the working environment.
#'
#' @param scope Quoted name or acronym of the project.
#' @param abstractFile Quoted name and path of abstract (in markdown format)
#' @param methodsFile Quoted name and path of methods (in markdown format)
#' @param keywordsFile Quoted name and path of keywords (in csv format)
#' @param publicationDate Quoted ISO date - defaults to today's date
#'
#' @import EML
#'
#' @return EML dataset entity is returned.
#'
#' @export
#'
create_dataset <- function(scope = "LTER",
                           abstractFile = "abstract.md",
                           methodsFile = "methods.md",
                           keywordsFile = "keywords.csv",
                           publicationDate = NULL) {

  # confirm required components exist in R environment
  if (!exists("title")) { stop("missing title") }
  if (!exists("creators")) { stop("missing creator") }
  if (!exists("metadataProvider")) { stop("missing metadata provider") }
  if (!exists("coverage")) { stop("missing coverage") }
  if (!exists("packageIdent")) { stop("missing package identifier") }

  # read abstract
  tryCatch({
    abstract <- read_markdown(abstractFile)
  }, error = function(err) {
    print(paste("could not read abstract:", err))
  }) # close try catch - abstract

  # methods
  if (!exists("enhancedMethods")) {
    tryCatch({
      methods <- list(description = read_markdown(methodsFile))
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
    dataset$project <- list(capProject, ltrebProject) # cap and ltreb projects

  } else {

    stop("project scope is not recognized")

  }

  # add associated party if exists
  if (exists('associatedParty')) { dataset$associatedParty <- associatedParty }

  return(dataset)

}
