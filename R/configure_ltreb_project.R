#' @title configure_ltreb_project
#'
#' @description configure_ltreb_project establishes an EML::project with the
#'   elements: (1) personnel, (2) project abstract, and (3) funding (grant
#'   numbers).
#'
#' @details configure_ltreb_project establishes an EML::project with the
#'   elements: (1) personnel, (2) project abstract, and (3) funding (grant
#'   numbers).
#'
#' @import EML
#'
#' @return establishes in the R environment upon package load a EML::project
#'   entity specific to the Sycamore Creek LTREB. If appropriate to include in a
#'   dataset, 'ltrebProject' should be referenced with the project argument when
#'   constructing a EML::dataset.
#'

# project title -----------------------------------------------------------

projectTitle <- "Multiscale effects of climate variability and change on hydrologic regimes, ecosystem function, and community structure in a desert stream and its catchment"


# project personnel -------------------------------------------------------

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
  id = "ltreb.personnel.nancy.grimm",
  role = "Principal Investigator"
)

# John Sabo

johnOrcid <- EML::eml$userId(directory = "https://orcid.org")
johnOrcid$userId <- "https://orcid.org/0000-0001-5259-0709"

john <- EML::eml$personnel(
  individualName = EML::eml$individualName(
    givenName = "John",
    surName = "Sabo"),
  electronicMailAddress = "john.l.sabo@asu.edu",
  organizationName = "Arizona State University",
  userId = johnOrcid,
  id = "ltreb.personnel.john.sabo",
  role = "Co-Principal Investigator"
)

projectPersonnel <- list(nancy, john)


# project abstract --------------------------------------------------------

projectAbstract <- list(
  markdown = "The primary objective of this project is to understand how long-term climate variability and change, as expressed through hydrologic regime shifts, influence the structure and function of desert streams. Desert streams are well suited for observing the consequences of climate variability because they experience high hydrologic variability at multiple scales. Researchers will determine how regime shifts influence 1) large-scale stream biogeomorphic structure (i.e., prevalence and persistence of wetlands) over multiple years via their influence on factors that control vegetation biomass, and 2) within-year successional patterns in ecosystem processes and community structure of primary producers and consumers of reaches along a gradient of wetland presence and stability. Arid regions are characterized by high interannual variation in precipitation, and these climate patterns drive the overall disturbance regime (in terms of flooding and drying), which influences the geomorphic structure and nutrient status of desert stream ecosystems. Embedded within the multi-annual hydrologic regime, flash floods scour stream channels and initiate a series of rapid successional changes by benthic algae, aquatic and wetland plants, and macroinvertebrates at short time scales (i.e., within a year); patterns of succession are hypothesized to vary among years within the long-term hydrologic regime. The research will use new techniques developed by the team to analyze the relationships between hydrologic variability, hydrologic regime shifts, and ecosystem and community properties."
)

# project funding ---------------------------------------------------------

projectFunding <- list(
  markdown = "
  - NSF SUCCESSION-I 1977-1979 DEB-7724478 (Fisher)
  - NSF SUCCESSION-II 1980-1983 DEB-8004145 (Fisher)
  - NSF SUCCESSION-III 1984-1987 BSR-8406891 (Fisher)
  - NSF Postdoc 1987-1989 BSR 87-00122 (Grimm)
  - NSF STABILITY - 1989-1992 BSR-8818612 (Fisher and Grimm)
  - NSF TROPHIC STRUCTURE 1990-1992 BSR-9008114 (Fisher, Grimm, and Dudley)
  - NSF LTREB I - 1991-1996 DEB-9108362 (Grimm and Fisher)
  - NSF HETEROGENEITY - 1993-1998 DEB-9306909 (Fisher and Grimm)
  - EPA HYPORHEIC - 1994-1996 #R821250-01-0 (Fisher and Grimm)
  - NSF LINX I - 1996-1999 - DEB-9628860 (subaward to Grimm, Marti, and Fisher)
  - NSF LTREB II - 1996-2001 DEB-9615358 (Grimm and Fisher)
  - NSF LINX II - 2001-2006 DEB-0111410 (subaward to Grimm and Dahm)
  - NSF LTREB III - 2009-2015 DEB-0918262 (Grimm and Sabo)
  - NSF LTREB IV - 2015-2020 DEB-1457227 (Grimm and Sabo)
  - NSF LINKAGES - 1998-2000 DEB-9727311 (Fisher and Grimm)
  - NSF DDIG - 1998-1999 DEB-9800912 (Dent and Grimm)
  "
)


# EML::project ------------------------------------------------------------

#' @export ltrebProject
ltrebProject <- list(
  title = projectTitle,
  personnel = projectPersonnel,
  abstract = projectAbstract,
  funding = projectFunding,
  award = list(
    funderName = "National Science Foundation",
    funderIdentifier = "https://ror.org/021nxhr62",
    awardNumber = "1457227",
    title = "LTREB Renewal: Multiscale effects of climate variability and change on hydrologic regimes, ecosystem function, and community structure in a desert stream and its catchment",
    awardUrl = "https://nsf.gov/awardsearch/showAward?AWD_ID=1457227&HistoricalAwards=false"
  )
)
