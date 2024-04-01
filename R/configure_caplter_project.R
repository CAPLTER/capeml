#' @title Configure CAP LTER project details for a EML project entity
#'
#' @description configure_caplter_project establishes an EML::project with the
#' elements: (1) personnel (CAP PI and Co-PIs), (2) project abstract (project
#' overview from CAP V proposal), and (3) funding (grant numbers for CAP I, II,
#' III, IV, and V).
#'
#' @details configure_caplter_project establishes an EML::project with the
#' elements: (1) personnel (CAP PI and Co-PIs), (2) project abstract (project
#' overview from CAP IV proposal), and (3) funding (grant numbers for CAP I,
#' II, III, IV, and V).
#'
#' @import EML
#'
#' @return establishes in the R environment a EML::project entity specific to
#' the CAP LTER.
#'
#' @export
#'
configure_caplter_project <- function() {

  # project title -----------------------------------------------------------

  projectTitle <- 'Central Arizona\u2013Phoenix Long-Term Ecological Research Project'


  # project personnel -------------------------------------------------------

  # Dan Childers

  danOrcid <- EML::eml$userId(directory = "https://orcid.org")
  danOrcid$userId <- "0000-0003-3904-0803"

  dan <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Daniel",
      surName   = "Childers"
    ),
    electronicMailAddress = "dan.childers@asu.edu",
    organizationName      = "Arizona State University",
    userId                = danOrcid,
    role                  = "Principal Investigator"
  )

  # Nancy Grimm

  nancyOrcid <- EML::eml$userId(directory = "https://orcid.org")
  nancyOrcid$userId <- "0000-0001-9374-660X"

  nancy <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Nancy",
      surName   = "Grimm"
    ),
    electronicMailAddress = "nbgrimm@asu.edu",
    organizationName      = "Arizona State University",
    userId                = nancyOrcid,
    role                  = "Co-principal Investigator"
  )

  # Sharon Hall

  sharonOrcid <- EML::eml$userId(directory = "https://orcid.org")
  sharonOrcid$userId <- "0000-0002-8859-6691"

  sharon <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = c("Sharon", "J"),
      surName = "Hall"
    ),
    electronicMailAddress = "sharonjhall@asu.edu",
    organizationName      = "Arizona State University",
    userId                = sharonOrcid,
    role                  = "Co-principal Investigator"
  )

  # Billie Turner

  billieOrcid <- EML::eml$userId(directory = "https://orcid.org")
  billieOrcid$userId <- "0000-0002-6507-521X"

  billie <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Billie",
      surName   = "Turner II"
    ),
    electronicMailAddress = "Billie.L.Turner@asu.edu",
    organizationName      = "Arizona State University",
    userId                = billieOrcid,
    role                  = "Co-principal Investigator"
  )

  # Abby York

  abbyOrcid <- EML::eml$userId(directory = "https://orcid.org")
  abbyOrcid$userId <- "0000-0002-2313-9262"

  abby <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Abigail",
      surName   = "York"
    ),
    electronicMailAddress = "Abigail.York@asu.edu",
    organizationName      = "Arizona State University",
    userId                = abbyOrcid,
    role                  = "Co-principal Investigator"
  )

  projectPersonnel <- list(dan, nancy, sharon, billie, abby)


  # project abstract --------------------------------------------------------

  projectAbstract <- "Humankind is increasingly an urban species and urban ecosystems are therefore profoundly important. Cities are concentrated consumers of energy and resources and producers of various wastes, but they are also centers of social networks, innovation, efficiency, and solutions. The Central Arizona?Phoenix Long Term Ecological Research program (CAP V) is a research project that includes scientists from a variety of disciplines focused on understanding cities as hybrid ecosystems including both environmental and human components, and their interactions. Understanding urban ecosystems remains the central focus of CAP V after 25 years of innovative research. The interconnectedness of human motivation, behavior, actions, and outcomes with urban ecosystem structure and function leads to a fundamental question addressed by this project. How have/are human-environment interactions mediated by urban ecological infrastructure to shape past, present, and future ecosystem functions? This project will further indicate how we can use knowledge of these relationships to inform more just, transformative, and sustainable futures. Broader societal impacts are intentionally integrated in CAP V research, with many activities involving explicit partnerships with practitioners and communities that historically have had little voice in the future of their city and environment. This work forms a translational link among social-ecological research outcomes, city institutions, and communities with the goal of ultimately making Phoenix, and cities in general, better and more sustainable places to live.

  CAP research will extend its focus on the theory of Urban Ecological Infrastructure (UEI) as a critical bridge between the system?s biophysical and human/social domains. CAP researchers will continue to explore interdisciplinary urban ecology in residential landscapes, urban waterbodies, desert parks and preserves, while examining the plants, animals, climate, urban design and governance across the metropolitan Phoenix region. New research initiatives will include a focus on environmental justice and equity, as well as urban air quality. The research will develop both knowledge and solutions in underserved and historically neglected communities, including local Indigenous communities. CAP V research is organized around five interdisciplinary areas: 1) ecosystem structure and function and biogeochemical cycling; 2) adaptation and eco-evolutionary dynamics; 3) urban climate, and air quality; 4) urban nature and human perceptions, decisions, and wellbeing; and 5) environmental justice, governance, and transformative futures. Investigating these questions, in the context of the previous 25 years of CAP research, will guide the CAP research endeavor towards more sustainable and resilient futures for U.S. cities and for our increasingly urban society.

  This award is jointly funded by the Division of Environmental Biology and the Division of Behavioral and Cognitive Sciences.

  This award reflects NSF's statutory mission and has been deemed worthy of support through evaluation using the Foundation's intellectual merit and broader impacts review criteria.
  "

  # project funding ---------------------------------------------------------

  projectFunding <- "NSF Awards: CAP I: DEB-9714833, CAP II: DEB-0423704, CAP III: DEB-1026865, CAP IV: DEB-1832016, CAP V: DEB-2224662"

  # EML::project ------------------------------------------------------------

  project <- list(
    title     = projectTitle,
    personnel = projectPersonnel,
    abstract  = projectAbstract,
    funding   = projectFunding,
    award     = list(
      funderName       = "National Science Foundation",
      funderIdentifier = "https://ror.org/021nxhr62",
      awardNumber      = "2224662",
      title            = "LTER: CAP V: Investigating how relationships between urban ecological infrastructure and human-environment interactions shape the structure and function of urban ecosystems",
      awardUrl         = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=2224662&HistoricalAwards=false"
    )
  )

  # return --------------------------------------------------------------------

  return(project)

}
