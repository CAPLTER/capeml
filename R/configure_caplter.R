#' @title configure_caplter
#'
#' @description configure_caplter establishes several data set elements that are
#'   likely to be consistent across all CAP LTER or GIOS data sets, including
#'   language, distribution, and access. In addition, address, publisher, and
#'   contact are established each specific to CAP LTER or GIOS. The appropriate
#'   reference (to CAP LTER or GIOS) should be made for these elements at the
#'   time of eml$dataset construction. Note that several elements apply to all
#'   projects. These include: giosLanguage, capRights, and lterAccess.
#'
#' @details configure_caplter establishes several data set elements that are
#'   likely to be consistent across all CAP LTER or GIOS data sets, including
#'   language, distribution, and access. In addition, address, publisher, and
#'   contact are established each specific to CAP LTER or GIOS. The appropriate
#'   reference (to CAP LTER or GIOS) should be made for these elements at the
#'   time of eml$dataset construction. Most of these eml entities are
#'   established in the R environment when the capeml package is loaded and
#'   available for reference in the EML development template. Distribution is
#'   the exception as this is a function that requires the data set package
#'   identifier as an input.
#'
#' @note Most entities are simply loaded when the package is loaded without the
#' need to call them explicitly. An exception is the create_distribution()
#' function.
#'
#' @import EML
#'
#' @return establishes in the R environment upon package load, eml entities
#'   referenced in (or outside) a EML development template.

# GIOS address, publisher, contact ----------------------------------------

#' @title default GIOS address
#' @export giosAddress
giosAddress <- EML::eml$address(
  deliveryPoint      = c("Arizona State University", "Global Institute of Sustainability and Innovation"),
  city               = "Tempe",
  administrativeArea = "AZ",
  postalCode         = "85287-5402",
  country            = "USA"
)

#' @title default GIOS publisher as EML::publisher
#' @export giosPublisher
giosPublisher <- EML::eml$publisher(
  organizationName = "Arizona State University, Julie Ann Wrigley Global Institute of Sustainability and Innovation",
  address = giosAddress)

#' @title default GIOS contact as EML::contact
#' @export giosContact
giosContact <- EML::eml$contact(
  organizationName      = "Julie Ann Wrigley Global Institute of Sustainability and Innovation, Arizona State University",
  positionName          = "Data Manager",
  electronicMailAddress = "caplter.data@asu.edu",
  onlineUrl             = "https://researchdata.asu.edu/",
  address               = giosAddress
)


# CAP LTER address, publisher, contact ------------------------------------

#' @title default CAP LTER address
#' @export capAddress
capAddress <- EML::eml$address(
  deliveryPoint      = c("Arizona State University", "Global Institute of Sustainability and Innovation"),
  city               = "Tempe",
  administrativeArea = "AZ",
  postalCode         = "85287-5402",
  country            = "USA"
)

#' @title default CAP LTER publisher as EML::publisher
#' @export capPublisher
capPublisher <- EML::eml$publisher(
  organizationName = "Central Arizona\u2013Phoenix LTER",
  address          = capAddress
)

#' @title CAP LTER ROR
#' @export cap_ror
cap_ror <- list(
  userId    = "020zjmd13",
  directory = "https://ror.org/"
)

#' @title default CAP LTER contact as EML::contact
#' @export capContact
capContact <- EML::eml$contact(
  organizationName      = "Central Arizona\u2013Phoenix LTER",
  positionName          = "Information Manager",
  electronicMailAddress = "caplter.data@asu.edu",
  onlineUrl             = "https://sustainability-innovation.asu.edu/caplter/",
  address               = capAddress,
  userId                = cap_ror
)


# language ----------------------------------------------------------------

#' @title giosLanguage: default language (english) passed to EML-generating workflow
#' @export giosLanguage
giosLanguage <- "english"


# intellectual rights -----------------------------------------------------

#' @title capRights: default intellectual rights statement passed to EML-generating workflow
#' @export capRights
capRights <- 'This data package is released to the "public domain" under Creative Commons CC0 1.0 "No Rights Reserved" (see: https://creativecommons.org/publicdomain/zero/1.0/). The consumer of these data ("Data User" herein) has an ethical obligation to cite it appropriately in any publication that results from its use. The Data User should realize that these data may be actively used by others for ongoing research and that coordination may be necessary to prevent duplicate publication. The Data User is urged to contact the authors of these data if any questions about methodology or results occur. Where appropriate, the Data User is encouraged to consider collaboration or coauthorship with the authors. The Data User should realize that misinterpretation of data may occur if used out of context of the original study. While substantial efforts are made to ensure the accuracy of data and associated documentation, complete accuracy of data sets cannot be guaranteed. All data are made available "as is". The Data User should be aware, however, that data are updated periodically and it is the responsibility of the Data User to check for new versions of the data. The data authors and the repository where these data were obtained shall not be liable for damages resulting from any use or misinterpretation of the data. Thank you.'


# license ----------------------------------------------------------------------

cap_licensed <- EML::eml$licensed(
  licenseName = "Creative Commons Zero v1.0 Universal",
  url         = "https://spdx.org/licenses/CC0-1.0",
  identifier  = "CC0-1.0"
)


# access ------------------------------------------------------------------

allow_cap <- EML::eml$allow(
  principal  = "uid=CAP,o=EDI,dc=edirepository,dc=org",
  permission = "all"
)

allow_public <- EML::eml$allow(
  principal  = "public",
  permission = "read"
)


#' @title default CAP LTER / GIOS access as EML::access
#' @export lterAccess
lterAccess <- EML::eml$access(
  authSystem = "knb",
  order      = "allowFirst",
  scope      = "document",
  allow      = list(
    allow_cap,
    allow_public
  )
)


# metadata distribution (i.e., path to xml file) ----
#' @title create_distribution
#' @param packageIdent The data package number
#' @export create_distribution
create_distribution <- function(packageIdent) {

  xml_url <- EML::eml$online(
    onlineDescription = "CAPLTER Metadata URL",
    url               = paste0("https://sustainability.asu.edu/caplter/data/data-catalog/view/", packageIdent, "/xml/")
  )

  metadata_dist <- EML::eml$distribution(online = xml_url)

}
