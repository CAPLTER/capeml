# helper: a project in a temporary working directory with data and metadata
# directories outside of it; returns the paths
local_project <- function(
  metadata_path = TRUE,
  data_path     = TRUE,
  env           = parent.frame()
  ) {

  root          <- withr::local_tempdir(.local_envir = env)
  project_dir   <- file.path(root, "project")
  data_dir      <- file.path(root, "data")
  metadata_dir  <- file.path(root, "metadata")

  dir.create(project_dir)
  dir.create(file.path(data_dir, "nested", "deeper"), recursive = TRUE)

  configuration <- list(
    scope       = "knb-lter-cap",
    identifier  = 1,
    baseURL     = "https://example.org/data/",
    title       = "locations dataset",
    project     = "gios",
    maintenance = "none"
  )

  if (data_path) configuration$data_path <- data_dir
  if (metadata_path) configuration$metadata_path <- metadata_dir

  yaml::write_yaml(configuration, file.path(project_dir, "config.yaml"))

  withr::local_dir(project_dir, .local_envir = env)

  list(
    project  = project_dir,
    data     = data_dir,
    metadata = metadata_dir
  )

}

# helper: fill required metadata in an attributes template
annotate_attributes <- function(path) {

  attrs <- yaml::read_yaml(path)
  if (is.character(attrs)) {
    attrs <- yaml::yaml.load(attrs)
  }

  attrs <- purrr::map(attrs, function(attribute) {
    attribute$attributeDefinition <- paste("definition of", attribute$attributeName)
    if (attribute$columnClasses == "numeric") {
      attribute$unit <- "meter"
    }
    attribute
  })

  yaml::write_yaml(attrs, path)

}

location_table <- data.frame(
  site   = factor(c("a", "b", "a")),
  height = c(1.5, 2, 3.25)
)


testthat::test_that("templates default to the working directory without config", {

  withr::local_dir(withr::local_tempdir())
  rlang::local_bindings(location_table = location_table, .env = globalenv())

  capeml::write_attributes(location_table)
  capeml::write_factors(location_table)

  testthat::expect_true(file.exists("location_table_attrs.yaml"))
  testthat::expect_true(file.exists("location_table_factors.yaml"))
  testthat::expect_identical(
    capeml::find_template("location_table", "attrs"),
    "./location_table_attrs.yaml"
  )

})


testthat::test_that("templates are written to and read from metadata_path", {

  paths <- local_project()
  rlang::local_bindings(location_table = location_table, .env = globalenv())

  capeml::write_attributes(location_table)
  capeml::write_factors(location_table)

  testthat::expect_true(file.exists(file.path(paths$metadata, "location_table_attrs.yaml")))
  testthat::expect_true(file.exists(file.path(paths$metadata, "location_table_factors.yaml")))
  testthat::expect_length(list.files(paths$project, pattern = "_attrs|_factors"), 0)

  annotate_attributes(file.path(paths$metadata, "location_table_attrs.yaml"))

  attribute_metadata <- capeml::read_attributes("location_table")
  testthat::expect_setequal(attribute_metadata$table$attributeName, c("site", "height"))
  testthat::expect_true(any(grepl("enumeratedDomain", names(unlist(attribute_metadata$eml)))))

})


testthat::test_that("an explicit metadata_path overrides config.yaml", {

  paths <- local_project()
  rlang::local_bindings(location_table = location_table, .env = globalenv())
  elsewhere <- file.path(dirname(paths$project), "elsewhere")

  capeml::write_attributes(location_table, metadata_path = elsewhere)

  testthat::expect_true(file.exists(file.path(elsewhere, "location_table_attrs.yaml")))
  testthat::expect_false(dir.exists(paths$metadata))

})


testthat::test_that("templates in more than one location are an error", {

  paths <- local_project()
  rlang::local_bindings(location_table = location_table, .env = globalenv())

  capeml::write_attributes(location_table)
  file.copy(file.path(paths$metadata, "location_table_attrs.yaml"), ".")

  testthat::expect_error(
    capeml::read_attributes("location_table"),
    "more than one location"
  )

  file.remove(file.path(paths$metadata, "location_table_attrs.yaml"))

  testthat::expect_error(
    capeml::write_attributes(location_table, overwrite = TRUE),
    "already exists at"
  )

})


testthat::test_that("find_data_file resolves direct, relative, and recursive paths", {

  paths <- local_project()

  writeLines("x", "in_project.csv")
  writeLines("x", file.path(paths$data, "top.csv"))
  writeLines("x", file.path(paths$data, "nested", "deeper", "deep.csv"))
  writeLines("x", file.path(paths$data, "nested", "twice.csv"))
  writeLines("x", file.path(paths$data, "nested", "deeper", "twice.csv"))

  testthat::expect_identical(capeml::find_data_file("in_project.csv"), "in_project.csv")
  testthat::expect_identical(
    capeml::find_data_file("top.csv"),
    file.path(paths$data, "top.csv")
  )
  testthat::expect_identical(
    capeml::find_data_file("deep.csv"),
    file.path(paths$data, "nested", "deeper", "deep.csv")
  )
  testthat::expect_identical(
    capeml::find_data_file("deeper/twice.csv"),
    file.path(paths$data, "nested", "deeper", "twice.csv")
  )
  testthat::expect_identical(
    basename(capeml::find_data_file("deeper", data_path = paths$data)),
    "deeper"
  )
  testthat::expect_error(capeml::find_data_file("twice.csv"), "multiple candidates")
  testthat::expect_error(capeml::find_data_file("absent.csv"), "could not find")

})


testthat::test_that("find_data_file searches multiple data roots", {

  root_a <- withr::local_tempdir()
  root_b <- withr::local_tempdir()
  dir.create(file.path(root_b, "sub"))
  writeLines("x", file.path(root_b, "sub", "only_b.csv"))

  testthat::expect_identical(
    capeml::find_data_file("only_b.csv", data_path = c(root_a, root_b)),
    file.path(root_b, "sub", "only_b.csv")
  )

})


testthat::test_that("a missing data_path in config.yaml is an error", {

  local_project()
  configuration <- yaml::read_yaml("config.yaml")
  configuration$data_path <- "/no/such/directory"
  yaml::write_yaml(configuration, "config.yaml")

  testthat::expect_error(capeml::get_locations(), "data_path in config.yaml not found")

})


testthat::test_that("create_dataTable reads templates from metadata_path", {

  paths <- local_project()
  units_metadata_path <- NULL
  testthat::local_mocked_bindings(
    write_units = function(..., metadata_path) units_metadata_path <<- metadata_path,
    .package    = "capeml"
  )

  writeLines(
    c("site,height", "a,1.5", "b,2", "a,3.25"),
    file.path(paths$data, "nested", "heights.csv")
  )
  heights <- utils::read.csv(
    capeml::find_data_file("heights.csv"),
    stringsAsFactors = TRUE
  )
  rlang::local_bindings(heights = heights, .env = globalenv())

  capeml::write_attributes(heights)
  capeml::write_factors(heights)
  annotate_attributes(file.path(paths$metadata, "heights_attrs.yaml"))

  heights_DT <- capeml::create_dataTable(
    dfname        = heights,
    description   = "heights",
    metadata_path = paths$metadata
  )

  testthat::expect_identical(heights_DT$physical$objectName, "1_heights.csv")
  testthat::expect_true(file.exists("1_heights.csv"))
  testthat::expect_length(heights_DT$attributeList$attribute, 2)
  testthat::expect_identical(units_metadata_path, paths$metadata)
  testthat::expect_true(any(grepl("enumeratedDomain", names(unlist(heights_DT$attributeList)))))

})


testthat::test_that("create_otherEntity resolves data_path and does not write to it", {

  paths <- local_project()
  source_file <- file.path(paths$data, "nested", "notes.txt")
  writeLines("field notes", source_file)
  data_before <- list.files(paths$data, recursive = TRUE)

  notes_OE <- capeml::create_otherEntity(
    target_file_or_directory = "notes.txt",
    description              = "field notes"
  )

  testthat::expect_identical(notes_OE$physical$objectName, "1_notes.txt")
  testthat::expect_true(file.exists("1_notes.txt"))
  testthat::expect_identical(list.files(paths$data, recursive = TRUE), data_before)

})


testthat::test_that("create_entities_from_yaml passes a per-entity metadata_path", {

  paths <- local_project(metadata_path = FALSE)
  testthat::local_mocked_bindings(
    write_units = function(...) invisible(NULL),
    .package    = "capeml"
  )
  rlang::local_bindings(location_table = location_table, .env = globalenv())
  entity_metadata <- file.path(dirname(paths$project), "entity_metadata")

  capeml::write_attributes(location_table, metadata_path = entity_metadata)
  annotate_attributes(file.path(entity_metadata, "location_table_attrs.yaml"))

  entity <- capeml::create_entities_from_yaml(
    list(
      type          = "table",
      dfname        = "location_table",
      description   = "location table",
      overwrite     = TRUE,
      projectNaming = FALSE,
      metadata_path = entity_metadata
    )
  )

  testthat::expect_identical(entity$type, "table")
  testthat::expect_identical(entity$entity$physical$objectName, "location_table.csv")

})


testthat::test_that("create_otherEntity respects overwrite for the renamed copy", {

  local_project(data_path = FALSE, metadata_path = FALSE)
  writeLines("version one", "notes.txt")

  capeml::create_otherEntity(
    target_file_or_directory = "notes.txt",
    description              = "field notes"
  )

  writeLines("version two", "notes.txt")

  testthat::expect_error(
    capeml::create_otherEntity(
      target_file_or_directory = "notes.txt",
      description              = "field notes"
    ),
    "already exists in working directory"
  )
  testthat::expect_identical(readLines("1_notes.txt"), "version one")

  notes_OE <- capeml::create_otherEntity(
    target_file_or_directory = "notes.txt",
    description              = "field notes",
    overwrite                = TRUE
  )

  testthat::expect_identical(readLines("1_notes.txt"), "version two")
  testthat::expect_identical(
    unname(notes_OE$physical$authentication$authentication),
    unname(tools::md5sum("1_notes.txt"))
  )

})


testthat::test_that("create_otherEntity rejects directories and treats zip files as files", {

  local_project(data_path = FALSE, metadata_path = FALSE)
  dir.create("shapes")
  file.create(file.path("shapes", c("sites.shp", "sites.dbf")))

  testthat::expect_error(
    capeml::create_otherEntity(
      target_file_or_directory = "shapes",
      description              = "sites"
    ),
    "is a directory"
  )

  utils::zip("shapes.zip", files = list.files("shapes", full.names = TRUE), flags = "-jq")

  shapes_OE <- capeml::create_otherEntity(
    target_file_or_directory = "shapes.zip",
    description              = "sites",
    project_naming           = FALSE
  )

  testthat::expect_identical(shapes_OE$physical$objectName, "shapes.zip")
  testthat::expect_identical(
    shapes_OE$physical$dataFormat$externallyDefinedFormat$formatName,
    "Esri Shapefile (zipped)"
  )
  testthat::expect_true(file.exists("shapes.zip"))

})
