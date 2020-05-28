

create_dataset <- function() {

  # confirm presence of required components

  if (!exists("title")) { stop("missing title") }
  if (!exists("pubDate")) { stop("missing publication date") }
  if (!exists("creators")) { stop("missing creator") }
  if (!exists("metadataProvider")) { stop("missing metadata provider") }
  if (!exists("abstract")) { stop("missing abstract") }
  if (!exists("keywords")) { stop("missing keywords") }
  if (!exists("coverage")) { stop("missing coverage") }
  if (!exists("methods")) { stop("missing methods") }
  if (!exists("packageIdent")) { stop("missing package identifier") }

  # construct base dataset with required components
  dataset <- EML::eml$dataset(
    title = title,
    creator = creators,
    pubDate = pubDate,
    metadataProvider = metadataProvider,
    intellectualRights = capRights,
    abstract = abstract,
    keywordSet = keywords,
    coverage = coverage,
    methods = methods,
    distribution = create_distribution(packageIdent)
  )

    # contact = capContact, # cap contact
    # publisher = capPublisher, # cap pub

    # contact = giosContact, # gios contact
    # publisher = giosPublisher, # gios pub

    # project = capProject, # cap project
    # distribution = create_distribution(packageIdent)
    # close EML::eml$dataset

  if (exists('associatedParty')) { dataset$associatedParty <- associatedParty }

  return(dataset)

}
