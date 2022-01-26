library(capeml)
source("helper-create_dataTable.R")

testthat::test_that(
  desc = "create_dataTable (and internal function read_attributes()) returns a list",
  code = {

    expect_type(
      object = capeml::read_attributes(
        entity_name = "black_widow_behavior"
        ),
      type = "list"
    )

    expect_type(
      object = capeml::create_dataTable(
        dfname      = black_widow_behavior,
        description = black_widow_behavior_desc,
        overwrite   = TRUE
        ),
      type = "list"
    )

  }
)
