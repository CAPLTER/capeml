#' @title create EML entity of type dataTable
#'
#' @description create_dataTable generates a EML entity of type dataTable
#'
#' @details create_dataTable creates a EML dataTable object from a data frame
#'   or tibble object in the R environment. The function reads the attributes
#'   and classes contained within a supporting yaml file generated from the
#'   write_attributes function - create_dataTable will look for a file in the
#'   working directory with a name of type dataframeName_attrs.yaml. Factors
#'   also are read from a supporting yaml file - create_dataTable will look for
#'   a file in the working directory with a name of type
#'   dataframeName_factors.yaml. If that exists, the factor details outlined in
#'   that file will be incorporated into the EML, else the EML will be built
#'   without factor metadata. Note that this functionality is predicated on the
#'   existence of a file containing metadata about any factors in the
#'   dataframe, that the file is in the working directory, and that the file
#'   matches the dataframe name precisely; the function does not look for
#'   variables of type factor in the dataframe. In addition to generating a EML
#'   entity of type dataTable, create_dataTable writes the R object to file as
#'   type csv, renames the file with package number + base file name + md5sum +
#'   file extension (csv in this case), and deletes the original file.
#'
#' @note create_dataTable will look for a package number (packageNum) in
#'  config.yaml; this parameter is not passed to the function and it must exist.
#' @note create_dataTable currently accepts an argument for a base url path to
#'   which the new file name will be appended so as to be a web-resolvable file;
#'   the package defaults to a URL specific to the CAP LTER.
#'
#' @param dfname
#'  (character) The unquoted name of the R data frame or tibble.
#' @param description
#'   (character) A quoted description of the data frame or tibble that will
#'   populate the entityDescription element of the EML document. name of the
#'   raster data to be processed is included as a parameter in this metadata
#'   file).
#' @param dateRangeField (optional)
#'   (character) The quoted name of the data entity field that is a date field
#'   that would reflect the start and end dates of the data reflected in the
#'   data entity.
#' @param baseURL (optional)
#'   (character) The quoted base path of the web-accessible location of the data
#'   file; the name of the resulting file will be passed to the base path to
#'   generate a web-resolvable file path.
#' @param missingValueCode (optional)
#'   (character) create_dataTable will automatically document the presence of NA
#'   and NaN entries as missing values in the EML output. The user has the
#'   ability to identify an additional indicator of missing values (e.g.,
#'   "-9999", "missing").
#'
#' @import EML
#' @import dplyr
#' @importFrom tools md5sum file_ext
#' @importFrom purrr map_df
#' @importFrom tibble tibble add_row enframe
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom utils write.csv read.csv
#' @importFrom tidyr unnest_wider unnest_longer
#'
#' @return EML dataTable object is returned. Additionally, the data entity is
#'  written to file as type csv, and renamed with the package number + base
#'  file name + md5sum + file extension (csv in this case).
#'
#' @examples
#' \dontrun{
#'
#' data_entity <- read("data source") %>%
#'   processing %>%
#'   processing
#'
#' write_attributes(data_entity)
#' write_factors(data_entity)
#'
#' data_entity_desc <- "snow leopard data"
#'
#' # create_dataTable with minimal arguments
#' data_entity_DT <- create_dataTable(dfname = data_entity,
#'                                    description = data_entity_desc)
#'
#' # create_dataTable with optional arguments dateRangeField and missingValueCode
#' data_entity_DT <- create_dataTable(dfname = data_entity,
#'                                    description = data_entity_desc,
#'                                    dateRangeField = "observation date",
#'                                    missingValueCode = "missing")
#'
#' # The resulting dataTable entity can be added to a EML dataset
#' dataset <- EML::eml$dataset(dataTable = data_entity_DT)
#'
#' }
#'
#' @export

create_dataTable <- function(
  dfname,
  description,
  dateRangeField,
  baseURL = "https://data.gios.asu.edu/datasets/cap/",
  missingValueCode = NULL) {


  # get text reference of dataframe name for use throughout -------------------

  namestr <- deparse(substitute(dfname))


  # check for required entities -----------------------------------------------

  # config file
  if (!file.exists("config.yaml")) {

    stop("config.yaml not found")

  }

  # attributes file
  if (
    !file.exists(paste0(namestr, "_attrs.csv")) &
      !file.exists(paste0(namestr, "_attrs.yaml"))
    ) {

    stop("attributes table not found")

  }


  # file-level processing ---------------------------------------------------

  # retrieve package number from config.yaml
  packageNum <- yaml::yaml.load_file("config.yaml")$packageNum

  # 1. writes dataframe to file (csv) with temporary name;
  # 2. determines the md5sum of that file;
  # 3. removes temporary file;
  # 4. writes dataframe to file (csv) with convention:
  #    package id number + dataframe name + md5sum hash + csv;

  name_hash <- paste0(namestr, "hash")

  write.csv(
    x = dfname,
    file = paste0(name_hash, ".csv"),
    row.names = F,
    eol = "\r\n"
  )

  fname <- paste0(
    packageNum, "_",
    namestr, "_",
    tools::md5sum(paste0(name_hash, ".csv")),
    ".csv"
  )

  file.remove(paste0(name_hash, ".csv"))

  write.csv(
    x = dfname,
    file = fname,
    row.names = F,
    eol = "\r\n"
  )


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

  mvframe <- map_df(
    .x = colnames(dfname),
    .f = write_missing_values,
    dataObject = dfname,
    MVC = missingValueCode
  )


  # attributes --------------------------------------------------------------

  # load attributes from yaml or csv (default to yaml)
  if (file.exists(paste0(namestr, "_attrs.yaml"))) {

    attrs <- yaml::yaml.load_file(paste0(namestr, "_attrs.yaml"))
    attrs <- yaml::yaml.load(attrs)
    attrs <- tibble::enframe(attrs) %>%
      tidyr::unnest_wider(value) %>%
      dplyr::select(-one_of("name"))

  } else if (!file.exists(paste0(namestr, "_attrs.yaml")) && file.exists(paste0(namestr, "_attrs.csv"))) {

    attrs <- utils::read.csv(paste0(namestr, "_attrs.csv"))

  } else {

    stop(paste0("attributes file: ", namestr, "_attrs.yaml ", "not found in ", getwd()))

  }

  # column classes to vector (req'd by set_attributes)
  classes <- attrs %>%
    dplyr::pull(columnClasses)

  # copy attributeDefinition to defintion as appropriate;
  # remove col classes from attrs (req'd by set_attributes);
  # remove empty columns (real targets here are maximum and minimum, which can
  # throw an error for data without any numeric cols);
  # if character data exist, copy attributeDefinition to definition if a
  # definition is not provided

  # helper function to remove missing columns
  not_all_na <- function(x) {
    !all(is.na(x))
  }


  if (any(grepl("character", attrs$columnClasses, ignore.case = TRUE))) {

    attrs <- attrs %>%
      mutate(
        definition = case_when(
          grepl("character", columnClasses) & ((is.na(definition) | definition == "")) ~ attributeDefinition,
          TRUE ~ definition
        )
        ) %>%
    dplyr::select(-columnClasses) %>%
    dplyr::select_if(not_all_na)

  } else {

    attrs <- attrs %>%
      dplyr::select(-columnClasses) %>%
      dplyr::select_if(not_all_na)

  }

  #   attrs <- attrs %>%
  #     mutate(
  #       definition = case_when(
  #         grepl("character", columnClasses) & ((is.na(definition) | definition == "")) ~ attributeDefinition,
  #         TRUE ~ definition
  #       )
  #       ) %>%
  #   dplyr::select(-columnClasses) %>%
  #   dplyr::select_if(not_all_na)


  # factors ----------------------------------------------------------------------

  if (file.exists(paste0(namestr, "_factors.yaml"))) {

    df_factors <- yaml.load_file(paste0(namestr, "_factors.yaml")) %>%
      yaml::yaml.load() %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value) %>%
      tidyr::unnest_wider(attribute) %>%
      tidyr::unnest_longer(levels) %>%
      tidyr::unnest_wider(levels) %>%
      dplyr::select(-one_of("name"))

    has_factors <- TRUE

  } else if (file.exists(paste0(namestr, "_factors.csv"))) {

    df_factors <- utils::read.csv(paste0(namestr, "_factors.csv"))

    has_factors <- TRUE

  } else {

    has_factors <- FALSE

  }


  # compile components for attributeList of dataTable ----------------------------

  # condition: factors present, missing values NOT present
  if (has_factors == TRUE & nrow(mvframe) == 0) {

    attr_list <- EML::set_attributes(
      attributes = attrs,
      factors = df_factors,
      col_classes = classes
    )

    # condition: factors present, missing values present
  } else if (has_factors == TRUE & nrow(mvframe) >= 1) {

    attr_list <- EML::set_attributes(
      attributes = attrs,
      factors = df_factors,
      col_classes = classes,
      missingValues = mvframe
    )

    # condition: factors NOT present, missing values present
  } else if (has_factors == FALSE & nrow(mvframe) >= 1) {

    attr_list <- EML::set_attributes(
      attributes = attrs,
      col_classes = classes,
      missingValues = mvframe
    )

    # condition: factors NOT present, missing values NOT present
  } else {

    attr_list <- EML::set_attributes(
      attributes = attrs,
      col_classes = classes
    )

  }


  # set physical ------------------------------------------------------------

  dataTablePhysical <- EML::set_physical(
    objectName = fname,
    numHeaderLines = 1,
    recordDelimiter = "\\r\\n",
    quoteCharacter = "\"",
    url = paste0(baseURL, fname)
  )


  # create dataTable entity -------------------------------------------------

  newDT <- EML::eml$dataTable(
    entityName = fname,
    entityDescription = description,
    physical = dataTablePhysical,
    attributeList = attr_list,
    numberOfRecords = nrow(dfname),
    id = fname
  )


  # add temporalCoverage if appropriate -------------------------------------

  if (!missing(dateRangeField)) {

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

  message(paste0("created dataTable: ", fname))

  return(newDT)

} # close create_dataTable
