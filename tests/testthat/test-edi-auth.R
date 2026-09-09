testthat::test_that("edi_login accepts an existing EDI API key", {
  withr::local_envvar(EDI_API_KEY = "test-api-key")

  testthat::local_mocked_bindings(
    login = function(...) {
      stop("login should not be called")
    },
    .package = "EDIutils"
  )

  testthat::expect_invisible(capeml::edi_login())
})

testthat::test_that("edi_login registers an EDI API key when forced", {
  withr::local_envvar(EDI_API_KEY = "test-api-key")
  actual <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    login = function(key) {
      actual$key <- key
      invisible(NULL)
    },
    .package = "EDIutils"
  )

  testthat::expect_invisible(capeml::edi_login(force = TRUE))
  testthat::expect_identical(actual$key, "test-api-key")
})

testthat::test_that("edi_login requires an EDI API key", {
  withr::local_envvar(c(
    EDI_API_KEY = NA,
    EDI_USER = "legacy-user",
    EDI_PASS = "legacy-password"
  ))

  testthat::expect_error(
    capeml::edi_login(),
    "EDI API key not found. Set EDI_API_KEY in your .Renviron.",
    fixed = TRUE
  )
})