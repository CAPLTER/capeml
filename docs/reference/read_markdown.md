# Read a markdown file with proper formatting for inclusion in a EML document

read_markdown reads a markdown file and adds an appropriate tag and
formatting for proper display in a EML document.

## Usage

``` r
read_markdown(mdFile)
```

## Arguments

- mdFile:

  (character) Quoted name and path of markdown file to be read.

## Value

A named (markdown) list object where the list item is text with leading
and trailing new line markers.

## Details

The read_markdown function reads a markdown file, ensures the text has a
leading and trailing new line marker, and add this text to a named list
where markdown is the name of the list. When added to EML, the list name
add the \`markdown\` tag to the text read from the file. The leading and
trailing new line markers added to the text ensure that the markdown
tags are on their own lines to avoid misinterpretation. leading and
trailing new line markers.
