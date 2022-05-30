library(capeml)

testthat::test_that(
  desc = "read_package_configuration returns expected object types",
  code = {

    expect_type(
      object = capeml::read_package_configuration(),
      type   = "list"
    )

    expect_type(
      object = capeml::read_package_configuration()$identifier,
      type = "integer"
    )

  }
)
