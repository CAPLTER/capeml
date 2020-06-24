#' @title configure_som_project
#'
#' @description configure_som_project establishes an EML::project with the
#'   elements: (1) personnel, (2) project abstract, and (3) funding (grant
#'   numbers)
#'
#' @details configure_som_project establishes an EML::project with the
#'   elements: (1) personnel, (2) project abstract, and (3) funding (grant
#'   numbers).
#'
#' @import EML
#'
#' @return establishes in the R environment upon package load a EML::project
#'   entity specific to the LTER SOM Working Group.
#'

# project title -----------------------------------------------------------

projectTitle <- "Advancing soil organic matter research: Synthesizing
multi-scale observations, manipulations and models"


# project personnel -------------------------------------------------------

# William R. Wieder

willOrcid <- EML::eml$userId(directory = "https://orcid.org")
willOrcid$userId <- "https://orcid.org/0000-0001-7116-1985"

will <- EML::eml$personnel(
  individualName = EML::eml$individualName(
    givenName = "William",
    givenName = "R",
    surName = "Wieder"),
  electronicMailAddress = "wwieder@ucar.edu",
  organizationName = "Institute of Arctic and Alpine Research, University of
  Colorado Boulder and the Climate and Global Dynamics Laboratory, National
  Center for Atmospheric Research",
  userId = willOrcid,
  id = "personnel.william.wieder",
  role = "Co-Principal Investigator"
)

# Kate Lajtha

kateOrcid <- EML::eml$userId(directory = "https://orcid.org")
kateOrcid$userId <- "https://orcid.org/0000-0002-6430-4818"

kate <- EML::eml$personnel(
  individualName = EML::eml$individualName(
    givenName = "Kate",
    surName = "Lajtha"),
  electronicMailAddress = "lajthak@science.oregonstate.edu",
  organizationName = "Department of Crop and Soil Sciences, Oregon State
  University",
  userId = kateOrcid,
  id = "personnel.kate.lajtha",
  role = "Co-Principal Investigator"
)


projectPersonnel <- list(will, kate)


# project abstract --------------------------------------------------------

projectAbstract <- "Soil organic matter is a massive storehouse for carbon, as
well as a key regulator of nutrient cycling and soil quality in terrestrial
ecosystems, yet ecology lacks a full understanding of the controls on
stabilization and breakdown of soil organic matter. Two sets of competing
theories underlie models that adequately predict site-specific dynamics, but
result in different sets of predictions about the response of soil organic
matter to perturbations. Cross-site synthesis of long-term, studies,
particularly those incorporating experimental perturbations, provides an
opportunity to evaluate these theories under varying conditions of climate,
biological community, and topography, among other factors. This working group
is synthesizing soil organic matter data across 15 LTER sites and also includes
data and participants from Critical Zone Observatory (CZO) sites, Detrital
Input and Removal Treatments (DIRT) Network, and Nutrient Network (NutNET). The
group's goal is to refine and evaluate soil organic matter stabilization
theories and to produce a dataset that encompasses the impact of experimental
manipulations on soil organic matter at different sites."


# project funding ---------------------------------------------------------

projectFunding <- "Soil Organic Matter Synthesis Group supported through the
Long Term Ecological Research Network Office (LNO) (NSF award numbers 1545288
  and 1929393) and the National Center for Ecological Analysis and Synthesis
(UCSB)"


# EML::project ------------------------------------------------------------

#' @export capProject
somProject <- EML::eml$project(
  title = projectTitle,
  personnel = projectPersonnel,
  abstract = projectAbstract,
  funding = projectFunding)
