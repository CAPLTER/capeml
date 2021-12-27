context("create_dataTable()")
library(capeml)

testthat::test_that(
  desc = "create_dataTable (and internal function read_attributes()) returns a list",
  code = {

    black_widow_behavior <- readr::read_csv(
      file           = "Black_Widow_Behavior.csv",
      na             = c(".", ""),
      show_col_types = FALSE
      ) |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("Date"), ~ as.Date(x = ., format = "%m/%d/%y")),
      Habitat = as.factor(Habitat),
      Site = as.factor(Site)
      ) |>
    dplyr::filter(!is.na(ID))

    black_widow_behavior_desc <- "Western black widow spider characteristics and behaviors in the field and the laboratory"

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
