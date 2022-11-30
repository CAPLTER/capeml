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
