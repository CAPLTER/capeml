#' @title create_dataTable
#'
#' @description create_dataTable generates a EML entity of type dataTable
#'
#' @details create_dataTable creates a EML dataTable object from a data frame or
#'   Tibble object in the R environment. The function reads the attributes and
#'   classes contained within a supporting csv file generated from the
#'   write_attributes function - create_dataTable will look for a file in the
#'   working directory with a name of type dataframeName_attrs.csv. Factors also
#'   are read from a supporting csv file - create_dataTable will look for a file
#'   in the working directory with a name of type dataframeName_factors.csv. If
#'   that exists, the factor details outlined in that file will be incorporated
#'   into the EML, else the EML will be built without factor metadata. Note that
#'   this functionality is predicated on the existence of a file containing
#'   metadata about any factors in the dataframe, that that file is in the
#'   working directory, and that the file matches the dataframe name precisely;
#'   the function does not look for variables of types factor in the dataframe.
#'   In addition to generating a EML entity of type dataTable, create_dataTable
#'   writes the R object to file as type csv, renames the file with project id +
#'   base file name + md5sum + file extension (csv in this case), and deletes
#'   the original file.
#'
#' @note create_dataTable will look for a project id in the working environment;
#'   this parameter is not passed to the function and it must exist.
#' @note create_dataTable currently accepts an argument for a base url path to
#'   which the new file name will be appended so as to be a web-resolvable file;
#'   the package defaults to a URL specific to the CAP LTER.
#'
#' @param dfname The unquoted name of the R data frame or Tibble.
#' @param description A quoted description of the data frame or Tibble that will
#'   populate the entityDescription element of the EML document. name of the
#'   raster data to be processed is included as a parameter in this metadata
#'   file).
#' @param dateRangeField (optional) The quoted name of the data entity field
#'   that is a date field that would reflect the start and end dates of the data
#'   reflected in the data entity.
#' @param baseURL The base path of the web-accessible location of the data file;
#'   the name of the resulting file will be passed to the base path to generate
#'   a web-resolvable file path.
#'
#' @import EML
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom tools md5sum file_ext
#'
#' @return EML dataTable object is returned. Additionally, the data entity is
#'   written to file as type csv, and renamed with the project id + base file
#'   name + md5sum + file extension (csv in this case).
#'
#' @examples
#' \dontrun{
#'
#'  dataTableObject <- create_dataTable(
#'    dfname = data object in R env.,
#'    description = "data from the field study",
#'    dateRangeField = "collection_date")
#' }
#'
#' @export


create_dataTable <- function(dfname, description, dateRangeField, baseURL = "https://data.gios.asu.edu/datasets/cap/") {

  # Writes a datframe to file, determines the md5sum of that file, (re)writing
  # the dataframe with the project id number and m5sum hash in the file name.
  # The first file written, that included only the datatframe name as the
  # filename, is removed from the directory.
  namestr <- deparse(substitute(dfname))
  write.csv(dfname, paste0(namestr, ".csv"), row.names = F, eol = "\r\n")
  fname <- paste0(projectid, "_", namestr, "_", md5sum(paste0(namestr,".csv")), ".csv")
  write.csv(dfname, fname, row.names = F, eol = "\r\n")
  file.remove(paste0(namestr, ".csv"))

  # Read the attributes file and extract classes note that the na.strings =
  # character(0) argument preserves NA strings in the file as strings but also
  # has the effect of converting actual NAs and missing values to "".
  attrs <- read.csv(paste0(namestr, "_attrs.csv"),
                    header = TRUE, sep = ",",
                    quote = "\"",
                    as.is = TRUE,
                    na.strings = character(0),
                    stringsAsFactors = F)
  classes <- attrs[,"columnClasses"] # column classes => vector as required by set_attribute

  # Compile components for attributeList of dataTable; set factors to NA if they
  # are not relevant to this dataset.
  if(file.exists(paste0(namestr, "_factors.csv"))) {

    df_factors <- read_csv(paste0(namestr, "_factors.csv"),
                           col_types = cols()) # to suppress tibble output

    attr_list <- set_attributes(attributes = attrs, factors = df_factors, col_classes = classes)

  } else {

    attr_list <- set_attributes(attributes = attrs, col_classes = classes)

  }

  # set physical
  dataTablePhysical <- set_physical(objectName = fname,
                                    numHeaderLines = 1,
                                    recordDelimiter = "\\r\\n",
                                    quoteCharacter = "\"",
                                    url = paste0(baseURL, fname))


  # create dataTable entity
  newDT <- EML::eml$dataTable(
    entityName = fname,
    entityDescription = description,
    physical = dataTablePhysical,
    attributeList = attr_list,
    numberOfRecords = nrow(dfname),
    id = fname)

  # add temporalCoverage if appropriate
  if(!missing(dateRangeField)) {

    dataTableCoverage <- EML::eml$coverage(
      temporalCoverage = EML::eml$temporalCoverage(
        rangeOfDates = EML::eml$rangeOfDates(
          EML::eml$beginDate(
            calendarDate = format(min(dfname[[dateRangeField]]), "%Y-%m-%d")
          ),
          EML::eml$endDate(
            calendarDate = format(max(dfname[[dateRangeField]]), "%Y-%m-%d")
          )
        )
      )
    )

    newDT$coverage <- dataTableTemporal

  } # close temporalCoverage

  return(newDT)

} # close create_dataTable
