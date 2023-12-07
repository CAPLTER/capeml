library(capeml)
source("helper-black_widow_behavior.R")
source("helper-annuals_biomass.R")

# These tests at least temporarily suspended. I cannot work out how to get the tests to see the objects in the test environment. For example, the fail here is always that update_attributes cannot find the object_pointer...presumably because it cannot locate annuals_biomass, which is odd since min and max, for example, can find annuals_biomass. This entire test suite runs perfectly outside of testthat. Need to revisit with I have more time.

testthat::test_that(
  desc = "update_attributes performs update and preserved unchanged metadata",
  code = {

    # min and max in existing data
    old_min <- min(annuals_biomass$mass, na.rm = TRUE)
    old_max <- max(annuals_biomass$mass, na.rm = TRUE)

    # get max from OE attrs.yaml
    from_read     <- capeml::read_attributes(entity_name = annuals_biomass)[["table"]]
    max_from_read <- from_read[from_read$attributeName == "mass", ]$maximum

    # check that existing data and attrs values match
    testthat::expect_equal(
      object   = old_max,       # from csv (628.06)
      expected = max_from_read  # from existing attributes table (628.06)
    )

    # change max value in data
    new_max <- old_max + 100
    annuals_biomass[1, ]$mass <- new_max

    # update attrs with new max data
    updated_attrs <- capeml::update_attributes(entity_name = annuals_biomass, return_type = "attr")

    # get min and max from updated attrs.yaml
    min_from_update <- updated_attrs$mass$minimum
    max_from_update <- updated_attrs$mass$maximum

    # check that updated data and attrs values match
    testthat::expect_equal(
      object   = new_max,
      expected = max_from_update
    )

    # check that other metadata were preserved
    testthat::expect_equal(
      object   = old_min,
      expected = min_from_update
    )

  }
)


testthat::test_that(
  desc = "keep overlapping attributes but omit any attributes in metadata that are not part of the entity",
  code = {

    # note that black_widow_behavior_edited had to be created in
    # helper-create_dataTable.R in order to be available for this test (i.e.,
    # the entity was not accessible if created within this test block)

    suppressWarnings(
      update_test_yaml <- capeml::update_attributes(
        entity_name = black_widow_behavior_edited,
        return_type = "attr"
      )
    )

    update_test_yaml <- update_test_yaml |>
    dplyr::bind_rows()

    attribute_expected     <- "Habitat" %in% update_test_yaml[["attributeName"]]
    attribute_not_expected <- "test_column" %in% update_test_yaml[["attributeName"]]

    # when update_attributes encounters an attribute that is included in the
    # existing metadata but is not part of the entity that is the target of the
    # update, any overlapping columns should be updated in the updated
    # attribute metadta but attributes that are part of the existing metadata
    # but not an attribute in the current form of the data entity will be
    # omitted in the updated metadata.

    testthat::expect_true(object = attribute_expected)
    testthat::expect_false(object = attribute_not_expected)

  }
)


testthat::test_that(
  desc = "update_attributes aborts if attribute in entity but not existing metadata",
  code = {

    black_widow_behavior[["new_num"]] <- sample(1:1000, nrow(black_widow_behavior), replace = TRUE)
    black_widow_behavior[["new_chr"]] <- sample(letters, nrow(black_widow_behavior), replace = TRUE)

    # abort when entity attributes are not in existing documentation
    testthat::expect_error(
      object = capeml::update_attributes(
        entity_name = black_widow_behavior,
        return_type = "attributes"
      )
    )

  }
)
