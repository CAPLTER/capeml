testthat::test_that(
  desc = "create_dataset removes stale generated annotations before processing",
  code = {
    withr::with_tempdir({
      writeLines("[]", "annotations.yaml")

      testthat::expect_error(
        object = capeml::create_dataset(),
        regexp = "missing coverage"
      )

      testthat::expect_false(file.exists("annotations.yaml"))
    })
  }
)