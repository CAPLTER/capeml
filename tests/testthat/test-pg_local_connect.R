testthat::test_that("pg_local_connect is deprecated", {
  testthat::local_mocked_bindings(
    dbConnect = function(...) "connection",
    .package = "DBI"
  )

  testthat::expect_warning(
    connection <- capeml::pg_local_connect(user = "u", password = "p"),
    "deprecated"
  )
  testthat::expect_identical(connection, "connection")
})
