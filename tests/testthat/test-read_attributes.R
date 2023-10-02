testthat::test_that(
  desc = "read_attributes returns both eml and table return_types",
  code = {

    testthat::expect_s3_class(
      object = capeml::read_attributes(
        entity_name = "black_widow_behavior",
        entity_id   = "hash"
        )[["eml"]],
      class  = "emld"
    )

    testthat::expect_s3_class(
      object = capeml::read_attributes(
        entity_name = "black_widow_behavior",
        entity_id   = "hash"
        )[["table"]],
      class  = "data.frame"
    )

  }
)


testthat::test_that(
  desc = "expect similar data structure between data entity and read_attributes when return_type is a table",
  code = {

    testthat::expect_setequal(
      object = capeml::read_attributes(
        entity_name = "black_widow_behavior",
        entity_id   = "hash"
        )[["table"]][["attributeName"]],
      expected = colnames(black_widow_behavior)
    )

    testthat::expect_equal(
      object = nrow(
        capeml::read_attributes(
          entity_name = "black_widow_behavior",
          entity_id   = "hash"
          )[["table"]]
      ),
      expected = ncol(black_widow_behavior)
    )

  }
)
