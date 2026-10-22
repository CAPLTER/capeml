testthat::test_that("write_attributes handles logical columns as character metadata", {

  logical_example <- data.frame(
    Synthetic_flag = c(TRUE, FALSE, NA),
    missing_flag = rep(NA, 3)
  )

  assign("logical_example", logical_example, envir = globalenv())
  withr::defer(rm("logical_example", envir = globalenv()))

  attrs <- capeml::write_attributes(
    dfname = logical_example,
    return_type = "attributes"
  )

  purrr::walk(names(logical_example), function(attribute_name) {
    testthat::expect_equal(
      attrs[[attribute_name]][["attributeName"]], attribute_name
    )
    testthat::expect_equal(
      attrs[[attribute_name]][["columnClasses"]], "character"
    )
    testthat::expect_equal(attrs[[attribute_name]][["definition"]], "")
    testthat::expect_null(attrs[[attribute_name]][["numberType"]])
  })

  testthat::expect_identical(
    get("logical_example", envir = globalenv()), logical_example
  )

})