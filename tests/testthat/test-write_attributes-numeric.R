testthat::test_that("numeric metadata handles columns with no finite values", {
  empty_columns <- list(
    missing_double = c(NA_real_, NA_real_),
    missing_integer = c(NA_integer_, NA_integer_),
    nonfinite = c(NA_real_, NaN, Inf, -Inf),
    empty_double = numeric(),
    empty_integer = integer()
  )

  purrr::iwalk(empty_columns, function(variable, attribute_name) {
    testthat::expect_warning(
      attrs <- capeml:::attributes_to_yaml(variable, attribute_name),
      NA
    )
    testthat::expect_identical(attrs$minimum, "")
    testthat::expect_identical(attrs$maximum, "")
    testthat::expect_identical(attrs$columnClasses, "numeric")
    testthat::expect_identical(
      attrs$numberType, if (is.integer(variable)) "integer" else "real"
    )
  })
})

testthat::test_that("numeric ranges and types use finite values", {
  numeric_columns <- list(
    natural = c(1, 2, NA, Inf),
    whole = c(0, 2, -Inf),
    integer = c(-2L, 1L, NA_integer_),
    real = c(-1.5, 2.5, NA, NaN, Inf, -Inf)
  )

  purrr::iwalk(numeric_columns, function(variable, number_type) {
    testthat::expect_warning(
      attrs <- capeml:::attributes_to_yaml(variable, number_type),
      NA
    )
    testthat::expect_identical(attrs$numberType, number_type)
    testthat::expect_equal(attrs$minimum, min(variable[is.finite(variable)]))
    testthat::expect_equal(attrs$maximum, max(variable[is.finite(variable)]))
  })
})

testthat::test_that("write_attributes writes no infinite numeric YAML values", {
  numeric_example <- data.frame(
    veg_height = c(NA_real_, NA_real_, NA_real_),
    missing_integer = c(NA_integer_, NA_integer_, NA_integer_),
    nonfinite = c(Inf, -Inf, NaN),
    mixed = c(1, Inf, 3)
  )

  assign("numeric_example", numeric_example, envir = globalenv())
  withr::defer(rm("numeric_example", envir = globalenv()))
  output_dir <- withr::local_tempdir()
  withr::local_dir(output_dir)

  testthat::expect_warning(capeml::write_attributes(numeric_example), NA)

  yaml_text <- readLines("numeric_example_attrs.yaml")
  testthat::expect_false(any(grepl("\\.inf|\\.nan", yaml_text, ignore.case = TRUE)))

  attrs <- yaml::read_yaml("numeric_example_attrs.yaml")
  if (is.character(attrs)) {
    attrs <- yaml::yaml.load(attrs)
  }

  testthat::expect_identical(attrs$veg_height$minimum, "")
  testthat::expect_identical(attrs$veg_height$maximum, "")
  testthat::expect_equal(attrs$mixed$minimum, 1)
  testthat::expect_equal(attrs$mixed$maximum, 3)
  testthat::expect_no_error(
    capeml::read_attributes_file("numeric_example", "numeric_entity")
  )
  testthat::expect_identical(
    get("numeric_example", envir = globalenv()), numeric_example
  )
})

testthat::test_that("numeric YAML bounds use fixed notation without losing precision", {
  fixed_example <- data.frame(
    gpstime = c(290599260, 290602340),
    fractional_gpstime = c(290599260.123456, 290602340.987654),
    small = c(1e-10, 2.345678901234567e-10),
    large = c(1e20, 1.2345678901234567e20),
    negative = c(-290602340.987654, -290599260.123456),
    integer = c(0L, 1L)
  )

  assign("fixed_example", fixed_example, envir = globalenv())
  withr::defer(rm("fixed_example", envir = globalenv()))
  output_dir <- withr::local_tempdir()
  withr::local_dir(output_dir)
  withr::local_options(list(digits = 3, scipen = -9, OutDec = ","))

  testthat::expect_warning(capeml::write_attributes(fixed_example), NA)

  yaml_text <- readLines("fixed_example_attrs.yaml")
  bounds_text <- grep("(minimum|maximum):", yaml_text, value = TRUE)
  testthat::expect_length(bounds_text, 2L * ncol(fixed_example))
  testthat::expect_false(any(grepl("[eE][+-]?[0-9]+", bounds_text)))
  testthat::expect_true(any(grepl("minimum: 290599260.0", bounds_text, fixed = TRUE)))
  testthat::expect_true(any(grepl("maximum: 290602340.0", bounds_text, fixed = TRUE)))

  attrs <- yaml::read_yaml("fixed_example_attrs.yaml")
  if (is.character(attrs)) {
    attrs <- yaml::yaml.load(attrs)
  }
  purrr::iwalk(fixed_example, function(variable, attribute_name) {
    testthat::expect_identical(attrs[[attribute_name]]$minimum, min(variable))
    testthat::expect_identical(attrs[[attribute_name]]$maximum, max(variable))
  })
  testthat::expect_no_error(
    capeml::read_attributes_file("fixed_example", "fixed_entity")
  )
  testthat::expect_identical(
    get("fixed_example", envir = globalenv()), fixed_example
  )
  testthat::expect_equal(getOption("digits"), 3)
  testthat::expect_equal(getOption("scipen"), -9)
  testthat::expect_identical(getOption("OutDec"), ",")
})