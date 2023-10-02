testthat::test_that(
  desc = "expect that get_custom_units executes and returns a character (list)",
  code = {

    testthat::expect_type(
      object = class(capeml::get_custom_units()),
      type   = "character"
    )

  }
)


testthat::test_that(
  desc = "expect appropriate dimension and content of get_custom_units",
  code = {

    testthat::expect_equal(
      object   = capeml::get_custom_units()[["metadata"]][["unitList"]][["unit"]][[1]][[1]][["id"]],
      expected = "MicroGM-PER-L"
    )

    testthat::expect_equal(
      object   = length(capeml::get_custom_units()[["metadata"]][["unitList"]][["unit"]][[1]]),
      expected = 4
    )

  }
)
