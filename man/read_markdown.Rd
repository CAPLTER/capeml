% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_markdown.R
\name{read_markdown}
\alias{read_markdown}
\title{Read a markdown file with proper formatting for inclusion in a EML
document}
\usage{
read_markdown(mdFile)
}
\arguments{
\item{mdFile}{(character) Quoted name and path of markdown file to be read.}
}
\value{
A named (markdown) list object where the list item is text with
  leading and trailing new line markers.
}
\description{
read_markdown reads a markdown file and adds an appropriate tag
  and formatting for proper display in a EML document.
}
\details{
The read_markdown function reads a markdown file, ensures the text
  has a leading and trailing new line marker, and add this text to a named
  list where markdown is the name of the list. When added to EML, the list
  name add the `markdown` tag to the text read from the file. The leading and
  trailing new line markers added to the text ensure that the markdown tags
  are on their own lines to avoid misinterpretation.
  leading and trailing new line markers.
}
