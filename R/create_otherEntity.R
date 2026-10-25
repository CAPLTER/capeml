#' @title create EML entity of type otherEntity
#'
#' @description \code{create_otherEntity} generates a EML entity of type
#' otherEntity.
#'
#' @details A otherEntity entity is created from a single file (e.g.,
#' desert_fertilization_sampling_sites.kml). The resulting entity is renamed
#' with the package number + base file name + file extension if project naming
#' is enabled. Directories are not supported; zip a directory before calling
#' \code{create_otherEntity} and pass the zip file. A zip file that contains a
#' shapefile (.shp) is documented with the format "Esri Shapefile (zipped)".
#'
#' @note \code{create_otherEntity} will look for a package identifier and
#' baseURL in config.yaml. `baseURL` must be provided. `package identifier`
#' must be provided if project naming is enabled.
#'
#' @note The target data file can be located anywhere on a local computer but
#' a renamed file will be written to the current working directory (the source
#' location is never written to). A
#' target that is not found as given is searched for in the directories
#' identified by \code{data_path} in config.yaml (see
#' \code{\link{find_data_file}}).
#'
#' @note Attribute and factor metadata (e.g., file_name_attrs.yaml) are read
#' from \code{metadata_path}, the working directory, or the directory of the
#' target file. For files that cannot be read into R (e.g., a zip file of
#' LiDAR point clouds in LAZ format), create
#' these templates with \code{\link{write_other_attributes}} and
#' \code{\link{write_other_factors}}; \code{create_otherEntity} stops with a
#' list of any required fields that are blank.
#'
#' @param target_file_or_directory
#'  (character) The quoted name and path of the data file (the argument name
#'  is retained for compatibility with existing data_objects.yaml files).
#' @param description
#'  (character) A description of the data entity.
#' @param overwrite
#'  (logical) If project naming is enabled, a logical indicating whether to
#'  overwrite an existing renamed file in the working directory.
#' @param project_naming
#'  (logical) Indicating if the file should be renamed per the style used by
#'  the CAP LTER (default) with the package number + base file name + file
#'  extension. The passed file name will be used if this parameter is set to
#'  FALSE.
#' @param additional_information
#'  (character) Additional information about the data object. Should be quoted,
#'  and accepts (but not required) markdown formatting.
#' @param entity_value_description
#'  (character) For raster data, a desciption of the raster value.
#' @param metadata_path
#'  (character) Directory of metadata templates. Defaults to
#'  \code{metadata_path} in config.yaml, else the working directory.
#'
#' @import EML
#' @importFrom dplyr select pull mutate select_if
#' @importFrom readr read_csv
#' @importFrom tools md5sum file_ext
#' @importFrom stringr str_extract
#' @importFrom tools file_ext
#' @importFrom utils unzip
#' @importFrom yaml yaml.load_file
#'
#' @return EML entity of type otherEntity is returned. Additionally, a copy of
#' the data file renamed with the package number + base file name + file
#' extension is written to the working directory if project naming is enabled.
#'
#' @examples
#' \dontrun{
#'
#'  # using default parameters
#'
#'  desert_fertilization_sites <- capeml::create_otherEntity(
#'    target_file_or_directory = "path-to-file/sampling_sites.Rmd",
#'    description              = "long-term study site documentation"
#'  )
#'
#'  # set overwrite = TRUE - has the effect of overwriting an existing renamed
#'  # copy of the file in the working directory.
#'
#'  pass_codebook_2011 <- capeml::create_otherEntity(
#'    target_file_or_directory = "PASS-2011-Codebook-Feb2016rev.pdf",
#'    description              = "PASS 2011 survey codebook",
#'    overwrite                = TRUE
#'  )
#'
#'  # example of a zipped directory without project naming:
#'
#'  max_temperature <- capeml::create_otherEntity(
#'    target_file_or_directory = "max_temperature.zip",
#'    description              = "rasters of max temperature years 2000-2016",
#'    project_naming           = FALSE
#'  )
#'
#' }
#'
#' @export
#'
create_otherEntity <- function(
  target_file_or_directory,
  description,
  overwrite              = FALSE,
  project_naming         = TRUE,
  additional_information = NULL,
  entity_value_description,
  metadata_path          = NULL
  ) {

  # required parameters --------------------------------------------------------

  # description

  if (missing("description")) {

    stop("please provide a description for this object")

  }

  # target file

  if (missing("target_file_or_directory")) {

    stop("specify the name of the file")

  }


  # resolve target in the working directory or data_path

  target_file_or_directory <- find_data_file(target_file_or_directory)
  source_directory         <- dirname(target_file_or_directory)

  # directories must be zipped before calling create_otherEntity

  if (dir.exists(target_file_or_directory)) {

    stop(
      "'", target_file_or_directory, "' is a directory, zip the directory and ",
      "pass the zip file to create_otherEntity"
    )

  }


  # flag zipped shapefiles

  is_shape <- grepl("zip", tools::file_ext(target_file_or_directory), ignore.case = TRUE) &&
    any(grepl("\\.shp$", utils::unzip(target_file_or_directory, list = TRUE)[["Name"]], ignore.case = TRUE))


  # object size, type, and hash ------------------------------------------------

  # determine the file extension
  fileExtension <- tools::file_ext(target_file_or_directory)

  # set authentication (md5)
  fileAuthentication                <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- tools::md5sum(target_file_or_directory)

  # set file size
  fileSize      <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(target_file_or_directory))

  # set file format
  if (grepl("pdf", tools::file_ext(target_file_or_directory), ignore.case = TRUE)) {

    fileDataFormat <- EML::eml$dataFormat(
      externallyDefinedFormat = EML::eml$externallyDefinedFormat(formatName = "Portable Document Format")
    )

  } else if (is_shape == TRUE) {

    fileDataFormat <- EML::eml$dataFormat(
      externallyDefinedFormat = EML::eml$externallyDefinedFormat(formatName = "Esri Shapefile (zipped)")
    )

  } else {

    fileDataFormat <- EML::eml$dataFormat(
      externallyDefinedFormat = EML::eml$externallyDefinedFormat(formatName = fileExtension)
    )

  }

  targetFileBaseName <- basename(target_file_or_directory)
  template_name      <- tools::file_path_sans_ext(targetFileBaseName)


  # retrieve dataset details from config.yaml

  configurations <- read_package_configuration()


  # project naming -------------------------------------------------------------

  if (project_naming == TRUE) {

    identifier <- configurations$identifier

    targetFileBaseName <- paste0(
      identifier, "_",
      stringr::str_extract(targetFileBaseName, "^[^\\.]*"),
      ".",
      fileExtension
    )

    # write renamed copy to the working directory so that the source
    # location (possibly read-only data_path) is not modified

    if (file.exists(targetFileBaseName) && !isTRUE(overwrite)) {

      stop(
        "file to be created (", targetFileBaseName, ") already exists in ",
        "working directory (set overwrite to TRUE)"
      )

    }

    copied <- file.copy(
      from      = target_file_or_directory,
      to        = targetFileBaseName,
      overwrite = TRUE
    )

    if (!copied) {

      stop("could not write ", targetFileBaseName, " to the working directory")

    }

  }


  # construct physical ---------------------------------------------------------

  fileURL <- configurations$fileURL

  fileDistribution <- EML::eml$distribution(
    EML::eml$online(url = paste0(fileURL, targetFileBaseName))
  )

  filePhysical <- EML::eml$physical(
    objectName     = targetFileBaseName,
    authentication = fileAuthentication,
    size           = fileSize,
    dataFormat     = fileDataFormat,
    distribution   = fileDistribution
  )


  # create otherEntity ---------------------------------------------------------

  newOE <- EML::eml$otherEntity(
    entityName        = targetFileBaseName,
    entityDescription = description,
    physical          = filePhysical,
    entityType        = fileExtension,
    id                = targetFileBaseName
  )


  # attributes and factors ------------------------------------------------------

  # templates (see write_other_attributes and write_other_factors) are read from
  # metadata_path, the working directory, or, for older projects, the directory
  # of the target file

  has_attributes <- !is.null(
    find_template(
      name          = template_name,
      type          = "attrs",
      metadata_path = metadata_path,
      also          = source_directory
    )
  )

  attrs   <- NULL
  classes <- NULL

  if (has_attributes) {

    attributes_from_file <- read_attributes_file(
      string_pointer = template_name,
      entity_id      = targetFileBaseName,
      metadata_path  = metadata_path,
      also           = source_directory
    )

    attrs   <- attributes_from_file[["attrs"]]
    classes <- attributes_from_file[["classes"]]

  }

  entity_factors <- read_factors_file(
    string_pointer = template_name,
    metadata_path  = metadata_path,
    also           = source_directory
  )

  has_factors <- !is.null(entity_factors)

  check_attribute_templates(
    attrs   = attrs,
    classes = classes,
    factors = entity_factors,
    source  = template_name
  )


  # units ----------------------------------------------------------------------

  if (has_attributes && "unit" %in% names(attrs) && any(!is.na(attrs[["unit"]]))) {

    capeml::write_units(provided_attributes_table = attrs)

  }


  # compile components for attributeList ---------------------------------------

  # condition: factors only

  if (has_attributes == FALSE && has_factors == TRUE) {

    if (missing(entity_value_description)) {

      entity_value_description <- "METADATA_NOT_PROVIDED"

    }

    attrs_factor_only <- data.frame(
      attributeName       = entity_factors[["attributeName"]][[1]],
      attributeDefinition = entity_value_description
    )

    attr_list <- EML::set_attributes(
      attributes  = attrs_factor_only,
      factors     = entity_factors,
      col_classes = "factor"
    )

    has_factor_only <- TRUE

    message("added: ", targetFileBaseName, " factors")

  } else {

    has_factor_only <- FALSE

  }

  # condition: attributes and factors

  if (has_attributes == TRUE && has_factors == TRUE) {

    attr_list <- EML::set_attributes(
      attributes  = attrs,
      factors     = entity_factors,
      col_classes = classes
    )

    message("added: ", targetFileBaseName, " attributes")
    message("added: ", targetFileBaseName, " factors")

  }

  # condition: attributes only

  if (has_attributes == TRUE && has_factors == FALSE) {

    attr_list <- EML::set_attributes(
      attributes  = attrs,
      col_classes = classes
    )

    message("added: ", targetFileBaseName, " attributes")

  }

  # add attributeList

  if (has_attributes == TRUE | has_factor_only == TRUE) {

    newOE$attributeList <- attr_list

  }


  # additional information -----------------------------------------------------

  if (!is.null(additional_information)) {

    newOE$additionalInfo <- additional_information

  }


  # return other entity object -------------------------------------------------

  message(
    "created otherEntity: ", targetFileBaseName,
    if (has_attributes == FALSE && has_factor_only == FALSE) " sans attributes"
  )
  return(newOE)

}
