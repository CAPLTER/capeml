library(capeml)
source("helper-black_widow_behavior.R")

# These tests at least temporarily suspended. I cannot work out how to get the tests to see the objects in the test environment. For example, the fail here is always that update_attributes cannot find the object_pointer...presumably because it cannot locate annuals_biomass, which is odd since min and max, for example, can find annuals_biomass. This entire test suite runs perfectly outside of testthat. Need to revisit with I have more time.

testthat::test_that(
  desc = "write_attributes performs appropriate action given return_type",
  code = {

    black_widow_behavior_temp <- black_widow_behavior

    withr::with_file(
      file = "black_widow_behavior_temp_attrs.yaml",
      code = {

        testthat::expect_no_error(
          object = capeml::write_attributes(
            dfname      = black_widow_behavior_temp,
            overwrite   = TRUE,
            return_type = "yaml"
          )
        )

        testthat::expect_no_warning(
          object = capeml::write_attributes(
            dfname      = black_widow_behavior_temp,
            overwrite   = TRUE,
            return_type = "yaml"
          )
        )

        testthat::expect_error(
          object = capeml::write_attributes(
            dfname      = does_not_exist,
            overwrite   = TRUE,
            return_type = "yaml"
          )
        )

        testthat::expect_true(
          object = file.exists("black_widow_behavior_temp_attrs.yaml")
        )

        testthat::expect_type(
          object = capeml::write_attributes(
            dfname      = black_widow_behavior_temp,
            overwrite   = TRUE,
            return_type = "attr"
            ),
          type = "list"
        )

        testthat::expect_error(
          object = capeml::write_attributes(
            dfname      = black_widow_behavior_temp,
            overwrite   = TRUE,
            return_type = "text"
            ),
          regexp = "ambiguous return_type, should be 'yaml' or 'attributes'"
        )

        from_yaml <- capeml::write_attributes(
          dfname      = black_widow_behavior_temp,
          return_type = "attr"
        )

        testthat::expect_equal(
          object = from_yaml[["Habitat"]][["attributeName"]],
          expected = "Habitat"
        )

      }
    )
  }
)
