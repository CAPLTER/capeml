write_version_config <- function(version = NULL) {
  config <- list(
    scope = "knb-lter-cap",
    identifier = 1L,
    baseURL = "https://example.org/data/",
    title = "Hard-coded version dataset",
    project = "gios",
    maintenance = "none"
  )
  config$version <- version
  yaml::write_yaml(config, "config.yaml")
}

# any path to the EDI API fails loudly
local_no_edi_api <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    edi_login = function(...) stop("edi_login should not be called"),
    get_next_version = function(...) stop("get_next_version should not be called"),
    .package = "capeml",
    .env = env
  )
  testthat::local_mocked_bindings(
    list_data_package_scopes = function(...) stop("EDI API should not be called"),
    list_data_package_revisions = function(...) stop("EDI API should not be called"),
    .package = "EDIutils",
    .env = env
  )
}

testthat::test_that("read_package_configuration returns a hard-coded version", {
  withr::local_dir(withr::local_tempdir())

  write_version_config(version = 3)
  testthat::expect_identical(capeml::read_package_configuration()$version, 3L)

  write_version_config()
  testthat::expect_null(capeml::read_package_configuration()$version)
})

testthat::test_that("read_package_configuration rejects invalid versions", {
  withr::local_dir(withr::local_tempdir())

  purrr::walk(list(0, -1, 1.5, "abc", c(1, 2)), function(bad_version) {
    write_version_config(version = bad_version)
    testthat::expect_error(
      capeml::read_package_configuration(),
      "positive whole number"
    )
  })
})

testthat::test_that("resolve_version uses a hard-coded version without the EDI API", {
  local_no_edi_api()

  testthat::expect_message(
    version <- resolve_version(list(scope = "edi", identifier = 1L, version = 4L)),
    "EDI repository not queried"
  )
  testthat::expect_identical(version, 4L)
})

testthat::test_that("resolve_version falls back to get_next_version", {
  testthat::local_mocked_bindings(
    get_next_version = function(provided_scope, provided_identifier, display_message) {
      testthat::expect_identical(provided_scope, "edi")
      testthat::expect_identical(provided_identifier, 1L)
      testthat::expect_true(display_message)
      9L
    },
    .package = "capeml"
  )

  testthat::expect_identical(
    resolve_version(list(scope = "edi", identifier = 1L), display_message = TRUE),
    9L
  )
})

testthat::test_that("workflow runs to completion with a hard-coded version and no EDI API", {
  withr::local_dir(withr::local_tempdir())
  local_no_edi_api()

  write_version_config(version = 5)
  writeLines("Hard-coded version dataset.", "abstract.md")
  writeLines("Version is set in config.yaml.", "methods.md")
  utils::write.csv(
    data.frame(thesaurus = "test", keyword = "vegetation", type = "theme"),
    "keywords.csv",
    row.names = FALSE
  )

  testthat::local_mocked_bindings(
    get_people = function(...) {
      list(
        creators = list(list(individualName = list(surName = "Researcher"))),
        metadata_providers = list(),
        associated_parties = list()
      )
    },
    .package = "capeml"
  )
  rlang::local_bindings(
    coverage = EML::eml$coverage(
      geographicCoverage = EML::eml$geographicCoverage(
        geographicDescription = "Phoenix, Arizona",
        boundingCoordinates = EML::eml$boundingCoordinates(
          westBoundingCoordinate = -112,
          eastBoundingCoordinate = -111,
          northBoundingCoordinate = 34,
          southBoundingCoordinate = 33
        )
      )
    ),
    .env = globalenv()
  )

  testthat::expect_message(
    dataset <- capeml::create_dataset(publication_date = "2026-10-26"),
    "using version 5 from config.yaml"
  )
  rlang::local_bindings(dataset = dataset, .env = globalenv())

  testthat::expect_message(eml <- capeml::create_eml(), "EDI repository not queried")
  testthat::expect_identical(eml$packageId, "knb-lter-cap.1.5")

  testthat::expect_message(capeml::write_cap_eml(eml), "EDI repository not queried")
  testthat::expect_true(file.exists("knb-lter-cap.1.5.xml"))
})

testthat::test_that("workflow queries EDI for the version when none is hard-coded", {
  withr::local_dir(withr::local_tempdir())
  write_version_config()

  testthat::local_mocked_bindings(
    get_next_version = function(...) 2L,
    .package = "capeml"
  )
  rlang::local_bindings(
    dataset = EML::eml$dataset(title = "Hard-coded version dataset"),
    .env = globalenv()
  )

  eml <- capeml::create_eml()
  testthat::expect_identical(eml$packageId, "knb-lter-cap.1.2")
})

testthat::test_that("write_config writes version only when supplied", {
  withr::local_dir(withr::local_tempdir())

  capeml::write_config(identifier = 716)
  testthat::expect_null(yaml::read_yaml("config.yaml")$version)

  capeml::write_config(identifier = 716, version = 3, overwrite = TRUE)
  testthat::expect_identical(capeml::read_package_configuration()$version, 3L)
})
