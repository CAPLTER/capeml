% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_people.R
\name{get_people}
\alias{get_people}
\title{translate personnel metadata to eml}
\usage{
get_people()
}
\value{
lists of data personnel appropriate for inclusion in a EML <dataset>
as `EML::eml$creator`, `EML::eml$metadataProvider`, and
`EML::eml$associatedParty` as relevant
}
\description{
\code{get_people} harvests metadata pertaining to deataset
personnel (people) documented in a `people.yaml` file and translates those
metadata to a lists appropriate for inclusion in a EML <dataset> as
`EML::eml$creator`, `EML::eml$metadataProvider`, and
`EML::eml$associatedParty` as relevant.
}
