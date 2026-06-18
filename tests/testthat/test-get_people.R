library(capeml)

testthat::test_that(
  desc = "get_person_attributes fails clearly for ambiguous and missing matches",
  code = {

    people_lookup <- data.frame(
      last_name    = c("Khan", "Khan", "Buo"),
      first_name   = c("Waqar", "Waqar Hassan", "Isaac"),
      middle_name  = c("H", NA, NA),
      organization = c("Arizona State University", "Arizona State University", "Arizona State University"),
      email        = c("wkhan17@asu.edu", "wkhan17@asu.edu", "ibuo1@asu.edu"),
      orcid        = c("0000-0003-0785-2508", "0000-0003-0785-2508", "0000-0002-6211-4957")
    )

    csv_path <- tempfile(fileext = ".csv")
    utils::write.csv(
      x         = people_lookup,
      file      = csv_path,
      row.names = FALSE,
      quote     = TRUE
    )

    testthat::expect_error(
      object = capeml:::get_person_attributes(
        this_last_name   = "khan",
        this_first_name  = "waq",
        this_data_source = csv_path
      ),
      regexp = "ambiguous person match in data source: last_name='khan', first_name='waq'; matched 2 records",
      fixed  = FALSE
    )

    testthat::expect_error(
      object = capeml:::get_person_attributes(
        this_last_name   = "middel",
        this_first_name  = "ari",
        this_data_source = csv_path
      ),
      regexp = "person not found in data source: last_name='middel', first_name='ari'",
      fixed  = FALSE
    )

  }
)
