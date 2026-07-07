library(capeml)

testthat::test_that(
  desc = "create_package uses supplied version without auto login",
  code = {

    actual <- new.env(parent = emptyenv())

    testthat::local_mocked_bindings(
      edi_login = function(force = FALSE) {
        stop("edi_login should not be called")
      },
      get_next_version = function(...) {
        stop("get_next_version should not be called")
      },
      .package = "capeml"
    )

    testthat::local_mocked_bindings(
      create_data_package = function(eml, env) {
        actual$eml <- eml
        actual$env <- env
        invisible(NULL)
      },
      .package = "EDIutils"
    )

    capeml::create_package(
      identifier = 1L,
      scope = "edi",
      version = 7L
    )

    testthat::expect_identical(actual$eml, "edi.1.7.xml")
    testthat::expect_identical(actual$env, "staging")

  }
)