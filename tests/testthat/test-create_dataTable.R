library(capeml)
source("helper-create_dataTable.R")

testthat::test_that(
  desc = "create_dataTable returns and writes expected output",
  code = {

    withr::with_file(
      file = "693_black_widow_behavior.csv",
      code = {

        testthat::expect_type(
          object = capeml::create_dataTable(
            dfname        = black_widow_behavior,
            description   = black_widow_behavior_desc,
            overwrite     = TRUE,
            projectNaming = TRUE
            ),
          type  = "list"
        )

        testthat::expect_true(
          object = file.exists("693_black_widow_behavior.csv")
        )

      }
    ) # close withr

  }
) # close test_that


testthat::test_that(
  desc = "appropriate file name when project naming is FALSE",
  code = {

    withr::with_file(
      file = "black_widow_behavior.csv",
      code = {

        testthat::expect_type(
          object = capeml::create_dataTable(
            dfname        = black_widow_behavior,
            description   = black_widow_behavior_desc,
            overwrite     = TRUE,
            projectNaming = FALSE
            ),
          type  = "list"
        )

        testthat::expect_true(
          object = file.exists("black_widow_behavior.csv")
        )

      }
    ) # close withr

  }
) # close test_that
