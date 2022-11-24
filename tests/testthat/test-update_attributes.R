testthat::test_that(
  desc = "update_attributes returns appropriate type given return_type and whether entity_name is quoted",
  code = {

    testthat::expect_type(
      object = capeml::update_attributes(
        entity_name = black_widow_behavior,
        return_type = "attr"
        ),
      type = "list"
    )

    testthat::expect_type(
      object = capeml::update_attributes(
        entity_name = "black_widow_behavior",
        return_type = "attr"
        ),
      type = "list"
    )

    testthat::expect_error(
      object = capeml::update_attributes(
        entity_name = black_widow_behavior,
        return_type = "text"
        ),
      regexp = "ambiguous return_type, should be 'yaml' or 'attributes'"
    )

  }
)


testthat::test_that(
  desc = "update_attributes performs update and preserved unchanged metadata",
  code = {

    old_max <- max(black_widow_behavior["First Lab Assay Mass"], na.rm = TRUE)
    new_max <- old_max + 0.5
    black_widow_behavior[1,]["First Lab Assay Mass"] <- new_max

    from_read <- capeml::read_attributes(
      entity_name = black_widow_behavior,
      return_type = "attributes"
    )

    max_from_read <- from_read[grepl("First Lab Assay Mass", from_read$attributeName, ignore.case = TRUE),][["maximum"]]

    # check that existing values match
    testthat::expect_equal(
      object   = old_max,
      expected = max_from_read
    )

    updated_attributes <- capeml::update_attributes(
      entity_name = black_widow_behavior,
      return_type = "attributes"
    )

    max_from_read <- updated_attributes[["First Lab Assay Mass"]][["maximum"]]

    # check that updated value is reflected
    testthat::expect_equal(
      object   = new_max,
      expected = max_from_read
    )

    # check that other metadata were preserved
    testthat::expect_equal(
      object   = updated_attributes[["Habitat"]][["attributeDefinition"]],
      expected = "habitat classification"
    )

  }
)
