testthat::test_that(
  desc = "expect that get_annotations executes and returns a character (list)",
  code = {

    testthat::expect_type(
      object = class(capeml::get_annotations()),
      type   = "character"
    )

  }
)


testthat::test_that(
  desc = "expect appropriate dimension and content of get_annotations",
  code = {

    testthat::expect_equal(
      object   = capeml::get_annotations()[["annotation"]][[1]][["references"]],
      expected = "05c13ff53c9cbbc19dcdc3acecd30052_10"
    )

    testthat::expect_equal(
      object   = length(capeml::get_annotations()[["annotation"]]),
      expected = 10
    )

  }
)
