testthat::test_that("create_taxonomicCoverage produces expected output", {

  file.copy(
    from      = testthat::test_path("data", "taxa_map.csv"),
    to        = "taxa_map.csv",
    overwrite = TRUE
  )

  capeml::create_taxonomicCoverage()

  testthat::expect_true(file.exists("taxonomicCoverage.xml"))

  expected <- readLines("taxonomicCoverageTest.xml")
  actual   <- readLines("taxonomicCoverage.xml")

  # remove ref to packageid
  expected <- expected[!grepl("packageId=", expected)]
  actual   <- actual[!grepl("packageId=", actual)]

  testthat::expect_equal(actual, expected)

# withr::with_tempdir({ do not use withr in this context })

}) # close testthat::test_that