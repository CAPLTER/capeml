% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_package_configuration.R
\name{read_package_configuration}
\alias{read_package_configuration}
\title{Read information from project configuration file ("config.yaml")}
\usage{
read_package_configuration()
}
\value{
A list of configuration details.
}
\description{
The read_package_configuration function reads the project
configuration details from the config.yaml file that must exist in the
working directory. This is a internal, convenience function intended mostly
for the utility of other capeml functions but read_package_configuration can
be run independenty.
}
\note{
This function if a bit of overkill in the sense that other capeml
functions (e.g., create_dataTable) could eaisily access the configuration
details by reading the config.yaml file. The purpose here is mostly to
accommodate the evolving structure of the config.yaml structure. For
example, capeml formerly took a projectIdentifier that featured scope +
dataset number + version. Logic is included here to accommodate older and
newer config.yaml styles; this logic is better encapsulated into this single
function rather than every place configuration details are accessed.
}
