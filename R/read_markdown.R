#' @title read_markdown
#'
#' @description read_markdown reads a markdown file and adds an appropriate tag
#'   and formatting for proper display in a EML document.
#'
#' @details The read_markdown function reads a markdown file, ensures the text
#'   has a leading and trailing new line marker, and add this text to a named
#'   list where markdown is the name of the list. When added to EML, the list
#'   name add the `markdown` tag to the text read from the file. The leading and
#'   trailing new line markers added to the text ensure that the markdown tags
#'   are on their own lines to avoid misinterpretation.
#'   leading and trailing new line markers.
#'
#' @param mdFile  Quoted name and path of markdown file to be read.
#'
#' @importFrom readr read_file
#'
#' @return A named (markdown) list object where the list item is text with
#'   leading and trailing new line markers.
#'
#' @export
#'
read_markdown <- function(mdFile) {

  text <- read_file(mdFile)

  if (!grepl("^\r|^\n", text)) { text <- paste0("\n", text) }
  if (!grepl("\r$|\n$", text)) { text <- paste0(text, "\n") }

  text <- list(markdown = text)

  return(text)

}
