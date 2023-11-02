#' @title Construct a csv tabular data file and create metadata of type
#' dataTable
#'
#' @description \code{create_dataTable} writes a tabular data file of type csv
#' and generates a EML entity of type dataTable
#'
#' @details \code{create_dataTable} writes a tabular data file of type csv and
#' creates a corresponding EML dataTable object from a data frame or tibble
#' object in the R environment. The function reads the attributes and classes
#' contained within a supporting yaml file generated from
#' \code{write_attributes} function - \code{create_dataTable} will look for a
#' file in the working directory with a name of type
#' data_object_name_attrs.yaml. Factors also are read from a supporting yaml
#' file - \code{create_dataTable} will look for a file in the working directory
#' with a name of type data_object_name_factors.yaml. If that exists, the
#' factor details outlined in that file will be incorporated into the EML, else
#' the EML will be built without factor metadata. Note that this functionality
#' is predicated on the existence of a file containing metadata about any
#' factors in the dataframe, that the file is in the working directory, and
#' that the file matches the dataframe name precisely; the function does not
#' look for variables of type factor in the dataframe. In addition to
#' generating a EML entity of type dataTable, \code{create_dataTable} writes
#' the R object to file as type csv, and renames the file with package number +
#' base object name + file extension (csv in this case) if selected.
#'
#' @note If project naming is TRUE then \code{create_dataTable} will look for a
#' package number (packageNum) in `config.yaml`; this parameter is not passed
#' to the function and it must exist.
#'
#' @param dfname
#' (character) The quoted or unquoted name of the R data frame or tibble.
#' @param description
#' (character) A quoted description of the data frame or tibble that will
#' populate the entityDescription element of the EML document. name of the
#' raster data to be processed is included as a parameter in this metadata
#' file).
#' @param dateRangeField (optional)
#' (character) The quoted name of the data entity field that is a date field
#' that would reflect the start and end dates of the data reflected in the data
#' entity.
#' @param overwrite
#' (logical) A logical indicating whether to overwrite an existing file bearing
#' the same name as csv file as it is imported.
#' @param projectNaming
#' (logical) Logical indicating if the resulting csv file should be renamed per
#' the style used in the capeml ecosystem with the project id + base file name
#' + file extension. If set to false, the resulting csv will bear the name the
#' object in the R environment.
#' @param missingValueCode (optional)
#' (character) \code{create_dataTable} will automatically document the presence
#' of NA and NaN entries as missing values in the EML output. The user has the
#' ability to identify one additional indicator of missing values (e.g.,
#' "-9999", "missing").
#' @param additional_information
#' (character) Additional information about the data object. Should be quoted,
#' and accepts (but does not require) markdown formatting.
#'
#' @import EML
#' @importFrom tools md5sum
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom utils write.csv
#' @importFrom stringr str_extract
#' @importFrom rlang get_expr is_expression
#'
#' @return EML dataTable object is returned. Additionally, the data entity is
#'  written to file as type csv, and renamed with the package number + base
#'  file name + md5sum + file extension (csv in this case).
#'
#' @examples
#' \dontrun{
#'
#' mycars <- mtcars |>
#'   head()
#'
#' try({
#'   capeml::write_attributes(mycars)
#'   capeml::write_factors(mycars)
#' })
#'
#' # create_dataTable with optional arguments dateRangeField and missingValueCode
#' mycars_DT <- capeml::create_dataTable(
#'  dfname      = mycars,
#'  description = "just a few cars",
#'  )
#'
#' }
#'
#' @export
#'
create_dataTable <- function(
  dfname,
  description,
  dateRangeField         = NULL,
  overwrite              = FALSE,
  projectNaming          = TRUE,
  missingValueCode       = NULL,
  additional_information = NULL
  ) {

  # get text reference of dataframe name for use throughout -------------------

  if (rlang::is_expression(dfname)) {

    namestr <- rlang::get_expr(dfname)

  } else {

    namestr <- deparse(substitute(dfname))

  }


  # required parameters -------------------------------------------------------

  # do not proceed if a description is not provided

  if (missing("description")) {

    stop("please provide a description for this table")

  }


  # dataTables must have an attributes file

  if (
    !file.exists(paste0(namestr, "_attrs.csv")) &
    !file.exists(paste0(namestr, "_attrs.yaml"))
    ) {

    stop("attributes file not found")

  }


  # retrieve dataset details from config.yaml

  configurations <- read_package_configuration()


  # project naming ------------------------------------------------------------

  if (projectNaming == TRUE) {

    project_name <- paste0(configurations$identifier, "_", namestr, ".csv")

  } else {

    project_name <- paste0(namestr, ".csv")

  }

  # load object from environment ----------------------------------------------

  data_object <- get(namestr)


  # write file ----------------------------------------------------------------

  if (file.exists(project_name) && overwrite == FALSE) {

    stop("csv file to be created (", paste0(project_name), ") already exists in working directory (set overwrite to TRUE)")

  }

  write.csv(
    x         = data_object,
    file      = project_name,
    row.names = FALSE,
    eol       = "\r\n"
  )


  # set physical ---------------------------------------------------------------

  # distribution

  fileURL <- configurations$fileURL

  fileDistribution <- EML::eml$distribution(
    EML::eml$online(url = paste0(fileURL, project_name))
  )

  # data format

  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat = EML::eml$externallyDefinedFormat(
      formatName = "Comma Separated Values Text")
  )

  # file size

  fileSize      <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(project_name))

  # authentication

  fileAuthentication                <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- tools::md5sum(project_name)

  # set physical ------------------------------------------------------------

  dataTablePhysical <- EML::set_physical(
    objectName      = project_name,
    numHeaderLines  = 1,
    recordDelimiter = "\\r\\n",
    quoteCharacter  = "\"",
    url             = paste0(fileURL, project_name)
  )


  # attributes ---------------------------------------------------------------

  attributes <- capeml::read_attributes(
    entity_name        = namestr,
    missing_value_code = missingValueCode,
    entity_id          = tools::md5sum(project_name)
  )[["eml"]]


  # units ---------------------------------------------------------------------

  if (!is.null(find_element(attributes, "unit"))) {

    capeml::write_units(
      entity_name = namestr,
      entity_id   = tools::md5sum(project_name)
    )

  }


  # create dataTable entity -------------------------------------------------

  newDT <- EML::eml$dataTable(
    entityName        = project_name,
    entityDescription = description,
    physical          = dataTablePhysical,
    attributeList     = attributes,
    numberOfRecords   = nrow(data_object),
    id                = project_name
  )


  # add temporalCoverage if appropriate -------------------------------------

  if (!is.null(dateRangeField)) {

    if (is.name(substitute(dateRangeField))) {

      dateRangeField <- deparse(substitute(dateRangeField))

    }

    if (dateRangeField %in% colnames(data_object)) {

      dataTableTemporalCoverage <- EML::eml$coverage(
        temporalCoverage = EML::eml$temporalCoverage(
          rangeOfDates = EML::eml$rangeOfDates(
            EML::eml$beginDate(
              calendarDate = format(min(data_object[[dateRangeField]], na.rm = TRUE), "%Y-%m-%d")
              ),
            EML::eml$endDate(
              calendarDate = format(max(data_object[[dateRangeField]], na.rm = TRUE), "%Y-%m-%d")
            )
          )
        )
      ) 

      newDT$coverage <- dataTableTemporalCoverage

    } else {

      message("dateRangeField not present in ", namestr)

    }

  } # close temporalCoverage


  # additional information -------------------------------------------------------

  if (!is.null(additional_information)) {

    newDT$additionalInfo <- additional_information

  }


  # closing message ---------------------------------------------------------

  message(paste0("created dataTable: ", project_name))


  # return ------------------------------------------------------------------

  return(newDT)

} # close create_dataTable


#' @description \code{find_element} is a helper function to
#' \code{create_dataTable} that checks for the presence of a particular element
#' in a list. In this case, we use \code{find_element} to determine if any of
#' the attributes of a datatable have units.

find_element <- function(attributes, element) {

  if (utils::hasName(attributes, element)) {

    attributes[[element]]

  } else if (is.list(attributes)) {

    for (obj in attributes) {

      ret <- Recall(obj, element)
      if (!is.null(ret)) return(ret)

    }

  } else {

    NULL

  }

}
