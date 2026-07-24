test_that("write_attributes handles time-like columns as character metadata", {

  herp_time_example <- tibble::tibble(
    observation_date = as.Date(c("2012-03-11", "2012-03-12")),
    time_start = structure(
      as.difftime(c("07:30:00", "08:15:00"), format = "%H:%M:%S"),
      class = c("hms", "difftime")
    ),
    time_end = structure(
      as.difftime(c("07:48:00", "08:29:00"), format = "%H:%M:%S"),
      class = c("hms", "difftime")
    )
  )

  assign("herp_time_example", herp_time_example, envir = globalenv())
  withr::defer(rm("herp_time_example", envir = globalenv()))

  attrs <- capeml::write_attributes(
    dfname      = herp_time_example,
    return_type = "attributes"
  )

  expect_equal(attrs[["time_start"]][["columnClasses"]], "character")
  expect_equal(attrs[["time_end"]][["columnClasses"]], "character")
  expect_equal(attrs[["observation_date"]][["columnClasses"]], "Date")

})