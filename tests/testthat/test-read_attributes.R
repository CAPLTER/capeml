testthat::test_that(
  desc = "read_attributes returns appropriate type given return_type",
  code = {

    testthat::expect_s3_class(
      object = capeml::read_attributes(
        entity_name = "black_widow_behavior",
        return_type = "eml"
        ),
      class  = "emld"
    )

    testthat::expect_s3_class(
      object = capeml::read_attributes(
        entity_name = "black_widow_behavior",
        return_type = "attr"
        ),
      class  = "data.frame"
    )

    testthat::expect_error(
      object = capeml::read_attributes(
        entity_name = "black_widow_behavior",
        return_type = "text"
        ),
      regexp = "ambiguous return_type, should be 'eml' or 'attributes'"
    )

  }
)


testthat::test_that(
  desc = "expect similar data structure between data entity and read_attributes when return_type = attributes",
  code = {

    black_widow_behavior <- readr::read_csv(file = "Black_Widow_Behavior.csv")

    testthat::expect_setequal(
      object = capeml::read_attributes(
        entity_name = "black_widow_behavior",
        return_type = "attr"
        )[["attributeName"]],
      expected = colnames(black_widow_behavior)
    )

    testthat::expect_equal(
      object = nrow(
        capeml::read_attributes(
          entity_name = "black_widow_behavior",
          return_type = "attr"
        )
        ),
      expected = ncol(black_widow_behavior)
    )

  }
)
