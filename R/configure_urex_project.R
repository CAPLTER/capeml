#' @title Configure UREx project details for a EML project entity
#'
#' @description configure_urex_project establishes an EML::project with the
#' elements: (1) personnel, (2) project abstract, and (3) funding
#'
#' @details  configure_urex_project establishes an EML::project with the
#' elements: (1) personnel, (2) project abstract, and (3) funding
#'
#' @import EML
#'
#' @return Establishes in the R environment a EML::project entity specific to
#' the UREx project.#'
#'
#' @export
#'
configure_urex_project  <- function() {

  # project title -----------------------------------------------------------

  projectTitle <- 'Urban resilience to extreme weather related events'


  # project personnel -------------------------------------------------------

  # Charles Redman

  chuckOrcid <- EML::eml$userId(directory = "https://orcid.org")
  chuckOrcid$userId <- "0000-0001-7193-7368"

  chuck <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Charles ",
      surName = "Redman"),
    electronicMailAddress = "CHARLES.REDMAN@asu.edu",
    organizationName = "Arizona State University",
    userId = chuckOrcid,
    role = "Principal Investigator"
  )

  # Nancy Grimm

  nancyOrcid <- EML::eml$userId(directory = "https://orcid.org")
  nancyOrcid$userId <- "0000-0001-9374-660X"

  nancy <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Nancy",
      surName = "Grimm"),
    electronicMailAddress = "nbgrimm@asu.edu",
    organizationName = "Arizona State University",
    userId = nancyOrcid,
    role = "Co-principal Investigator"
  )

  # P. Timon McPhearson

  timonOrcid <- EML::eml$userId(directory = "https://orcid.org")
  timonOrcid$userId <- "0000-0002-9499-0791"

  timon <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Timon",
      surName = "McPhearson"),
    electronicMailAddress = "timon.mcphearson@newschool.edu",
    organizationName = "The New School",
    userId = timonOrcid,
    role = "Co-principal Investigator"
  )

  # Tischa Munoz-Erickson

  # tischaOrcid <- EML::eml$userId(directory = "https://orcid.org")
  # tischaOrcid$userId <- ""

  tischa <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Tischa",
      surName = "Munoz-Erickson"),
    electronicMailAddress = "tamunozerickson@fs.fed.us",
    organizationName = "International Institute of Tropical Forestry, USDA Forest Service",
    # userId = tischaOrcid,
    role = "Co-principal Investigator"
  )

  # Mikhail Chester

  mikhailOrcid <- EML::eml$userId(directory = "https://orcid.org")
  mikhailOrcid$userId <- "0000-0002-9354-2102"

  mikhail <- EML::eml$personnel(
    individualName = EML::eml$individualName(
      givenName = "Mikhail",
      surName = "Chester"),
    electronicMailAddress = "mchester@asu.edu",
    organizationName = "Arizona State University",
    userId = mikhailOrcid,
    role = "Co-principal Investigator"
  )

  projectPersonnel <- list(chuck, nancy, timon, tischa, mikhail)


  # project abstract --------------------------------------------------------

  projectAbstract <- "Urban areas are vulnerable to extreme weather related events given their location, high concentration of people, and increasingly complex and interdependent infrastructure. Impacts of Hurricane Katrina, Superstorm Sandy, and other disasters demonstrate not just failures in built infrastructure, they highlight the inadequacy of institutions, resources, and information systems to prepare for and respond to events of this magnitude. The highly interdisciplinary and geographically dispersed Urban Resilience to Extremes Sustainability Research Network (UREx SRN) team will develop a diverse suite of new methods and tools to assess how infrastructure can be more resilient, provide ecosystem services, improve social well being, and exploit new technologies in ways that benefit all segments of urban populations. Starting with nine network cities (six continental U.S. and three Latin American, home to over 35 million residents) and expanding in future years, the vision of the UREx SRN is to co-produce the knowledge needed to promote resilient, livable cities in a future that will look very different from today. The extreme events that this project will focus on include urban flooding, coastal storms, regional droughts, and extreme heat waves. These events are already occurring with shocking frequency in U.S. and global cities. Infrastructure is viewed as an important line of defense against hazards and disasters, yet current urban infrastructure is aging and proving inadequate for protecting city populations. The UREx team will link SRN scientists, students, local practitioners, planners, industry, NGOs, and other stakeholders across >25 institutions and >70 collaborators to co-produce data, models, images, stories, and on-the-ground projects that show how a new resilient infrastructure can be developed. Infrastructure that is flexible, adaptable, safe-to-fail, socially equitable, and ecologically based will enhance urban resilience in the face of a higher incidence of extreme events, more culturally diverse communities, and continued urbanization pressures. Ultimately, the UREx SRN will help accelerate knowledge generation and application to encourage innovative strategies towards urban sustainability.

  The Urban Resilience to Extremes Sustainability Research Network (UREx SRN) will develop a novel theoretical framework for integrating social, ecological, and technological system (SETS) dimensions for conceptualizing, analyzing, and supporting urban infrastructure decisions in the face of climatic uncertainty in a more holistic way. The primary research question is: how do SETS domains interact to generate vulnerability or resilience to extreme weather related events, and how can urban SETS dynamics be guided along more resilient, equitable, and sustainable trajectories? The foundation of the network is eight working groups (WG) who will work together to answer this question. Network activities include: assembling comparable datasets for the cities; doing advanced climate and hydrological modeling and downscaling; conducting comparative analyses; further developing the SETS conceptual framework; experimenting with new visualization and computation approaches for representing the data and the SETS framework; using these products in participatory modeling and scenario analysis for each city; and developing the science and practice for transitioning infrastructure to meet 21st century resilience and sustainability goals. Continual network and educational evaluation will allow realignment and adjustment of the work based on iterative assessments. The program will develop a suite of interactive educational activities spanning institutions across the network, and including local practitioners as well as university students and young professionals. Working Groups include integral educational, communications, and diversity-enhancing activities for graduate and post-doctoral fellows, early-career researchers, and city professionals aimed at developing a model for co-producing effective and robust decision-support tools and educating the next generation of scientists and practitioners to carry out this work. These programs are expected to be especially attractive to Hispanic students and practitioners due to the project's focus on understanding the increasing cultural and intellectual connections of the U.S. and Latin America.

  The strategic goals of the UREx SRN are to:

  1)Build a network of cities, institutions, and student, post-doctoral, and faculty researchers to explore resilience of cities to extreme weather related events;

2)Develop novel theoretical frameworks that express a vision of sustainable, integrated urban infrastructure that is flexible, adaptable, safe-to-fail, socially equitable, and ecologically based;

3)Work with practitioners and decision makers, as well as a cadre of graduate and post-doctoral fellows, to co-produce knowledge that facilitates data-driven visioning and ultimately transitions to a sustainable future for urban infrastructure and, by extension, the fabric of urban social-ecological-technological sustainability; and

4)Create a model for incorporating assessment, learning, and adjustment in response to evaluative feedback in a large, transdisciplinary, multi-institutional, multi-national research network."


  # project funding ---------------------------------------------------------

  projectFunding <- "NSF Awards: CBET 1444755"


  # EML::project ------------------------------------------------------------

  project <- EML::eml$project(
    title = projectTitle,
    personnel = projectPersonnel,
    abstract = projectAbstract,
    funding = projectFunding
  )

  # return --------------------------------------------------------------------

  return(project)

}
