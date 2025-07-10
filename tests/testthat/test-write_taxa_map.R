testthat::test_that("write_taxa_map writes taxa_map.csv and returns correct output", {

  # Create a sample input data frame
  sample_taxa <- data.frame(
    scientific_name = c("Homo sapiens", "Panthera leo", NA, "carrot")
  )

  # Remove output file if it exists
  if (base::file.exists("taxa_map.csv")) {
    base::file.remove("taxa_map.csv")
  }

  # Run the function
  result <- capeml::write_taxa_map(sample_taxa, scientific_name)

  # Check that the output file exists
  testthat::expect_true(base::file.exists("taxa_map.csv"))

  # Check that the result is a data.frame
  testthat::expect_s3_class(result, "data.frame")

  # Check that the number of rows matches input
  testthat::expect_equal(base::nrow(result), base::nrow(sample_taxa))

  # Clean up
  base::file.remove("taxa_map.csv")

})