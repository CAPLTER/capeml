#' @title configure_caplter_project
#'
#' @description configure_caplter_project establishes an EML::project with the
#'   elements: (1) personnel (CAP PI and Co-PIs), (2) project abstract (project
#'   overview from CAP IV proposal), and (3) funding (grant numbers for CAP I,
#'   II, III, and IV).
#'
#' @details configure_caplter_project establishes an EML::project with the
#'   elements: (1) personnel (CAP PI and Co-PIs), (2) project abstract (project
#'   overview from CAP IV proposal), and (3) funding (grant numbers for CAP I,
#'   II, III, and IV).
#'
#' @import EML
#'
#' @return establishes in the R environment upon package load a EML::project
#'   entity specific to the CAP LTER. If appropriate to include in a dataset,
#'   'capProject' should be referenced with the project argument when
#'   constructing a EML::dataset.
#'

# project title -----------------------------------------------------------

projectTitle <- 'Central Arizona\u2013Phoenix Long-Term Ecological Research Project'


# project personnel -------------------------------------------------------

# Dan Childers

danOrcid <- EML::eml$userId(directory = "https://orcid.org")
danOrcid$userId <- "https://orcid.org/0000-0003-3904-0803"

dan <- EML::eml$personnel(
  individualName = EML::eml$individualName(
    givenName = "Daniel",
    surName = "Childers"),
  electronicMailAddress = "dan.childers@asu.edu",
  organizationName = "Arizona State University",
  userId = danOrcid,
  id = "caplter.personnel.daniel.childers",
  role = "Principal Investigator"
)

# Nancy Grimm

nancyOrcid <- EML::eml$userId(directory = "https://orcid.org")
nancyOrcid$userId <- "https://orcid.org/0000-0001-9374-660X"

nancy <- EML::eml$personnel(
  individualName = EML::eml$individualName(
    givenName = "Nancy",
    surName = "Grimm"),
  electronicMailAddress = "nbgrimm@asu.edu",
  organizationName = "Arizona State University",
  userId = nancyOrcid,
  id = "caplter.personnel.nancy.grimm",
  role = "Co-principal Investigator"
)

# Sharon Hall

sharonOrcid <- EML::eml$userId(directory = "https://orcid.org")
sharonOrcid$userId <- "https://orcid.org/0000-0002-8859-6691"

sharon <- EML::eml$personnel(
  individualName = EML::eml$individualName(
    givenName = "Sharon",
    surName = "Hall"),
  electronicMailAddress = "sharonjhall@asu.edu",
  organizationName = "Arizona State University",
  userId = sharonOrcid,
  id = "caplter.personnel.sharon.hall",
  role = "Co-principal Investigator"
)

# Billie Turner

billieOrcid <- EML::eml$userId(directory = "https://orcid.org")
billieOrcid$userId <- "https://orcid.org/0000-0002-6507-521X"

billie <- EML::eml$personnel(
  individualName = EML::eml$individualName(
    givenName = "Billie",
    surName = "Turner II"),
  electronicMailAddress = "Billie.L.Turner@asu.edu",
  organizationName = "Arizona State University",
  userId = billieOrcid,
  id = "caplter.personnel.billie.turner",
  role = "Co-principal Investigator"
)

# Abby York

abbyOrcid <- EML::eml$userId(directory = "https://orcid.org")
abbyOrcid$userId <- "https://orcid.org/0000-0002-2313-9262"

abby <- EML::eml$personnel(
  individualName = EML::eml$individualName(
    givenName = "Abigail",
    surName = "York"),
  electronicMailAddress = "Abigail.York@asu.edu",
  organizationName = "Arizona State University",
  userId = abbyOrcid,
  id = "caplter.personnel.abigail.york",
  role = "Co-principal Investigator"
)

projectPersonnel <- list(dan, nancy, sharon, billie, abby)


# project abstract --------------------------------------------------------

projectAbstract <- "Phase IV of the Central Arizona-Phoenix LTER (CAP) continues to focus on the question: How do the ecosystem services provided by urban ecological infrastructure (UEI) affect human outcomes and behavior, and how do human actions affect patterns of urban ecosystem structure and function and, ultimately, urban sustainability and resilience? The overarching goal is to foster social-ecological urban research aimed at understanding these complex systems using a holistic, ecology of cities perspective while contributing to an ecology for cities that enhances urban sustainability and resilience. This goal is being met through four broad programmatic objectives: (1) use long-term observations and datasets to articulate and answer new questions requiring a long-term perspective; (2) develop and use predictive models and future-looking scenarios to help answer research questions; (3) employ existing urban ecological theory while articulating new theory; and (4) build transdisciplinary partnerships to foster resilience and enhance sustainability in urban ecosystems while educating urban dwellers of all ages and experiences. CAP IV research is organized around eight interdisciplinary questions and ten long-term datasets and experiments, and researchers are organized into eight Interdisciplinary Research Themes to pursue these long-term research questions."


# project funding ---------------------------------------------------------

projectFunding <- "NSF Awards: CAP I: DEB-9714833, CAP II: DEB-0423704, CAP III: DEB-1026865, CAP IV: DEB-1832016"

# EML::project ------------------------------------------------------------

capProject <- list(
  title = projectTitle,
  personnel = projectPersonnel,
  abstract = projectAbstract,
  funding = projectFunding,
  award = list(
    funderName = "National Science Foundation",
    funderIdentifier = "https://ror.org/021nxhr62",
    awardNumber = "1832016",
    title = "LTER: CAP IV - Investigating urban ecology and sustainability through the lens of Urban Ecological Infrastructure",
    awardUrl = "https://nsf.gov/awardsearch/showAward?AWD_ID=1832016&HistoricalAwards=false"
  )
)
