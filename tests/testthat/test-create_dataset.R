testthat::test_that(
  desc = "create_dataset removes stale generated annotations before processing",
  code = {
    withr::with_tempdir({
      writeLines("[]", "annotations.yaml")

      testthat::expect_error(
        object = capeml::create_dataset(),
        regexp = "missing coverage"
      )

      testthat::expect_false(file.exists("annotations.yaml"))
    })
  }
)

testthat::test_that("create_dataset produces schema-valid EML with blank numeric bounds", {
  output_dir <- withr::local_tempdir()
  withr::local_dir(output_dir)

  yaml::write_yaml(
    list(
      scope = "knb-lter-cap",
      identifier = 1,
      baseURL = "https://example.org/data/",
      title = "Numeric bounds regression dataset",
      project = "gios",
      maintenance = "none"
    ),
    "config.yaml"
  )
  writeLines("Numeric bounds regression dataset.", "abstract.md")
  writeLines("Measurements include missing numeric values.", "methods.md")
  utils::write.csv(
    data.frame(thesaurus = "test", keyword = "vegetation", type = "theme"),
    "keywords.csv",
    row.names = FALSE
  )

  testthat::local_mocked_bindings(
    get_next_version = function(...) 1L,
    get_people = function(...) {
      list(
        creators = list(list(individualName = list(surName = "Researcher"))),
        metadata_providers = list(),
        associated_parties = list()
      )
    },
    .package = "capeml"
  )
  rlang::local_bindings(
    coverage = EML::eml$coverage(
      geographicCoverage = EML::eml$geographicCoverage(
        geographicDescription = "Phoenix, Arizona",
        boundingCoordinates = EML::eml$boundingCoordinates(
          westBoundingCoordinate = -112,
          eastBoundingCoordinate = -111,
          northBoundingCoordinate = 34,
          southBoundingCoordinate = 33
        )
      )
    ),
    .env = globalenv()
  )

  empty_numeric <- data.frame(
    veg_height = c(NA_real_, NA_real_),
    count = c(NA_integer_, NA_integer_)
  )
  scenarios <- list(
    empty = empty_numeric,
    mixed = cbind(empty_numeric, observed_height = c(1.5, 3))
  )

  purrr::iwalk(scenarios, function(data_object, scenario) {
    rlang::local_bindings(schema_numeric = data_object, .env = globalenv())
    testthat::expect_warning(
      capeml::write_attributes(schema_numeric, overwrite = TRUE), NA
    )

    attrs <- yaml::read_yaml("schema_numeric_attrs.yaml")
    if (is.character(attrs)) {
      attrs <- yaml::yaml.load(attrs)
    }
    attrs <- purrr::map(attrs, function(attribute) {
      attribute$attributeDefinition <- paste("Measurement of", attribute$attributeName)
      attribute$unit <- "meter"
      attribute
    })
    yaml::write_yaml(attrs, "schema_numeric_attrs.yaml")

    testthat::expect_warning(
      attribute_metadata <- capeml::read_attributes("schema_numeric"), NA
    )
    blank_rows <- attribute_metadata$table$attributeName %in% names(empty_numeric)
    if (scenario == "mixed") {
      testthat::expect_true(all(is.na(attribute_metadata$table$minimum[blank_rows])))
      testthat::expect_true(all(is.na(attribute_metadata$table$maximum[blank_rows])))
    } else {
      testthat::expect_false(any(c("minimum", "maximum") %in% names(attribute_metadata$table)))
    }

    testthat::expect_warning(
      data_table <- capeml::create_dataTable(
        schema_numeric,
        description = "Numeric measurements with missing values",
        projectNaming = FALSE,
        overwrite = TRUE
      ),
      NA
    )
    rlang::local_bindings(schema_numeric_DT = data_table, .env = globalenv())
    testthat::expect_warning(
      dataset <- capeml::create_dataset(publication_date = "2026-10-22"), NA
    )

    document <- EML::eml$eml(
      packageId = "knb-lter-cap.1.1",
      system = "knb",
      dataset = dataset
    )
    EML::write_eml(document, "numeric_bounds.xml")
    xml <- xml2::read_xml("numeric_bounds.xml")
    numeric_attributes <- xml2::xml_find_all(
      xml, ".//dataTable[entityName='schema_numeric.csv']/attributeList/attribute"
    )
    testthat::expect_length(numeric_attributes, ncol(data_object))
    empty_bounds <- xml2::xml_find_all(
      xml,
      ".//attribute[attributeName='veg_height' or attributeName='count']//numericDomain/bounds/*"
    )
    testthat::expect_length(empty_bounds, 0L)
    if (scenario == "mixed") {
      observed_bounds <- xml2::xml_find_all(
        xml, ".//attribute[attributeName='observed_height']//bounds/*"
      )
      testthat::expect_equal(as.numeric(xml2::xml_text(observed_bounds)), c(1.5, 3))
    }
    testthat::expect_false(any(grepl(
      "\\.inf|\\.nan", readLines("numeric_bounds.xml"), ignore.case = TRUE
    )))
    validation <- EML::eml_validate(
      "numeric_bounds.xml",
      schema = system.file("xsd/eml-2.2.0/eml.xsd", package = "emld")
    )
    testthat::expect_true(as.logical(validation), info = paste(attr(validation, "errors"), collapse = "\n"))
  })
})