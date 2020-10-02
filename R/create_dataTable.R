#' @title create EML entity of type dataTable
#'
#' @description create_dataTable generates a EML entity of type dataTable
#'
#' @details create_dataTable creates a EML dataTable object from a data frame or
#'   Tibble object in the R environment. The function reads the attributes and
#'   classes contained within a supporting csv file generated from the
#'   write_attributes function - create_dataTable will look for a file in the
#'   working directory with a name of type dataframeName_attrs.csv. Factors
#'   also are read from a supporting csv file - create_dataTable will look for
#'   a file in the working directory with a name of type
#'   dataframeName_factors.csv. If that exists, the factor details outlined in
#'   that file will be incorporated into the EML, else the EML will be built
#'   without factor metadata. Note that this functionality is predicated on the
#'   existence of a file containing metadata about any factors in the
#'   dataframe, that that file is in the working directory, and that the file
#'   matches the dataframe name precisely; the function does not look for
#'   variables of types factor in the dataframe. In addition to generating a
#'   EML entity of type dataTable, create_dataTable writes the R object to file
#'   as type csv, renames the file with package number + base file name +
#'   md5sum + file extension (csv in this case), and deletes the original file.
#'
#' @note create_dataTable will look for a package number (packageNum) in
#'  config.yaml; this parameter is not passed to the function and it must exist.
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
#' @param baseURL (optional) The base path of the web-accessible location of the
#'   data file; the name of the resulting file will be passed to the base path
#'   to generate a web-resolvable file path.
#' @param missingValueCode (optional)
#'  (character) create_dataTable will automatically document the presence of NA
#'  and NaN entries as missing values in the EML output. The user has the
#'  ability to identify an additional indicator of missing values (e.g.,
#'  "-9999", "missing").
#'
#' @import EML
#' @import dplyr
#' @importFrom tools md5sum file_ext
#' @importFrom purrr map_df
#' @importFrom tibble tibble add_row
#' @importFrom yaml yaml.load_file
#' @importFrom utils write.csv read.csv
#'
#' @return EML dataTable object is returned. Additionally, the data entity is
#'  written to file as type csv, and renamed with the package number + base file
#'  name + md5sum + file extension (csv in this case).
#'
#' @examples
#' \dontrun{
#' # data_entity <- read("data source") %>%
#' #   processing %>%
#' #   processing
#'
#' # write_attributes(data_entity)
#' # write_factors(data_entity)
#' #
#' # data_entity_desc <- "snow leopard data"
#'
#' # create_dataTable with minimal arguments
#' # data_entity_DT <- create_dataTable(dfname = data_entity,
#' #                                    description = data_entity_desc)
#'
#' # create_dataTable with optional arguments dateRangeField and missingValueCode
#' # data_entity_DT <- create_dataTable(dfname = data_entity,
#' #                                    description = data_entity_desc,
#' #                                    dateRangeField = "observation date",
#' #                                    missingValueCode = "missing")
#'
#' # The resulting dataTable entity can be added to a EML dataset
#' # dataset <- EML::eml$dataset(dataTable = data_entity_DT)
#' }
#'
#' @export

create_dataTable <- function(dfname,
                             description,
                             dateRangeField,
                             baseURL = "https://data.gios.asu.edu/datasets/cap/",
                             missingValueCode = NULL) {

  # file-level processing ---------------------------------------------------

  # retrieve package number from config.yaml
  if (!file.exists("config.yaml")) {
    stop("config.yaml not found")
  }
  packageNum <- yaml::yaml.load_file("config.yaml")$packageNum

  # Writes a datframe to file, determines the md5sum of that file, (re)writing
  # the dataframe with the package id number and m5sum hash in the file name.
  # The first file written, that included only the datatframe name as the
  # filename, is removed from the directory.
  namestr <- deparse(substitute(dfname))
  write.csv(dfname, paste0(namestr, ".csv"), row.names = F, eol = "\r\n")
  fname <- paste0(packageNum, "_", namestr, "_", tools::md5sum(paste0(namestr, ".csv")), ".csv")
  write.csv(dfname, fname, row.names = F, eol = "\r\n")
  file.remove(paste0(namestr, ".csv"))

  # write_missing_value -----------------------------------------------------

  missingValueFrame <- tibble(
    attributeName = as.character(),
    code = as.character(),
    definition = as.character()
  )

  write_missing_values <- function(dataObject, field, MVC) {

    if (any(is.na(dataObject[[field]]))) {

      missingValueFrame <- missingValueFrame %>%
        add_row(
          attributeName = field,
          code = "NA",
          definition = "missing value"
        )

    }

    if (any(is.nan(dataObject[[field]]))) {

      missingValueFrame <- missingValueFrame %>%
        add_row(
          attributeName = field,
          code = "NaN",
          definition = "missing value"
        )

    }

    if (any(dataObject[[field]] %in% MVC)) {

      missingValueFrame <- missingValueFrame %>%
        add_row(
          attributeName = field,
          code = MVC,
          definition = "missing value"
        )

    }

    return(missingValueFrame)

  }

  mvframe <- map_df(.x = colnames(dfname), .f = write_missing_values, dataObject = dfname, MVC = missingValueCode)

  # attributes --------------------------------------------------------------

  # Read the attributes file and extract classes into its own vector then delete
  # from attrs data frame (as required by rEML)
  attrs <- utils::read.csv(paste0(namestr, "_attrs.csv"))
  classes <- attrs %>% pull(columnClasses) # column classes to vector (req'd by set_attributes)
  attrs <- attrs %>% dplyr::select(-columnClasses) # remove col classes from attrs (req'd by set_attributes)

  # Compile components for attributeList of dataTable
  # condition: factors present, missing values not present
  if (file.exists(paste0(namestr, "_factors.csv")) & nrow(mvframe) == 0) {

    df_factors <- utils::read.csv(paste0(namestr, "_factors.csv"))

    attr_list <- set_attributes(attributes = attrs, factors = df_factors, col_classes = classes)

    # condition: factors present, missing values present
  } else if (file.exists(paste0(namestr, "_factors.csv")) & nrow(mvframe) >= 1) {

    df_factors <- utils::read.csv(paste0(namestr, "_factors.csv"))

    attr_list <- set_attributes(attributes = attrs, factors = df_factors, col_classes = classes, missingValues = mvframe)

    # condition: factors NOT present, missing values present
  } else if (!file.exists(paste0(namestr, "_factors.csv")) & nrow(mvframe) >= 1) {

    attr_list <- set_attributes(attributes = attrs, col_classes = classes, missingValues = mvframe)

    # condition: factors NOT present, missing values NOT present
  } else {

    attr_list <- set_attributes(attributes = attrs, col_classes = classes)

  }

  # set physical ------------------------------------------------------------

  dataTablePhysical <- set_physical(objectName = fname,
                                    numHeaderLines = 1,
                                    recordDelimiter = "\\r\\n",
                                    quoteCharacter = "\"",
                                    url = paste0(baseURL, fname))

  # create dataTable entity -------------------------------------------------

  newDT <- EML::eml$dataTable(
    entityName = fname,
    entityDescription = description,
    physical = dataTablePhysical,
    attributeList = attr_list,
    numberOfRecords = nrow(dfname),
    id = fname)

  # add temporalCoverage if appropriate -------------------------------------

  if(!missing(dateRangeField)) {

    dataTableTemporalCoverage <- EML::eml$coverage(
      temporalCoverage = EML::eml$temporalCoverage(
        rangeOfDates = EML::eml$rangeOfDates(
          EML::eml$beginDate(
            calendarDate = format(min(dfname[[dateRangeField]], na.rm = TRUE), "%Y-%m-%d")
          ),
          EML::eml$endDate(
            calendarDate = format(max(dfname[[dateRangeField]], na.rm = TRUE), "%Y-%m-%d")
          )
        )
      )
    )

    newDT$coverage <- dataTableTemporalCoverage

  } # close temporalCoverage

  return(newDT)

} # close create_dataTable
