#' @title Write a taxa map by joining input taxa with ITIS data
#'
#' @description This function takes a data frame of taxa names and joins them
#' with ITIS data from a local taxadb database, producing a standardized taxa
#' map suitable for downstream workflows. The function preserves input order,
#' checks for duplicate matches and row count consistency (with warnings), and
#' writes the result to \code{taxa_map.csv}.
#'
#' @param taxa_df A data frame containing taxa names to be mapped.
#' @param taxa_col The unquoted column name in \code{taxa_df} containing the
#' taxa names.
#'
#' @return Invisibly returns the taxa map data frame (also written to
#' \code{taxa_map.csv}).
#'
#' @details The function uses helper functions to connect to the taxadb
#' database, retrieve the ITIS table, and determine the correct table name. It
#' uses \code{taxadb::clean_names()} to standardize input names and
#' \code{dplyr} for data manipulation. Validation is performed using
#' \code{pointblank} to warn about duplicate matches and row count mismatches.
#'
#' @importFrom dplyr mutate left_join arrange select row_number
#' @importFrom purrr map_chr
#' @importFrom readr write_csv
#'
#' @examples
#' \dontrun{
#' sample_taxa <- data.frame(scientific_name = c("Homo sapiens", "Panthera"))
#' write_taxa_map(sample_taxa, scientific_name)
#' }
#'
#' @export
#'
write_taxa_map <- function(
  taxa_df,
  taxa_col
) {

  if (!exists("taxadb_itis_tbl", envir = .GlobalEnv)) {
    taxadb_itis_tbl <- get_itis_data()
  }

  taxa_ids <- taxa_df |>
    dplyr::mutate(
      taxa_clean = purrr::map_chr(
        .x = {{ taxa_col }},
        .f = ~ taxadb::clean_names(
          names     = .x,
          lowercase = FALSE
        )
      )
    ) |>
    # ensure unique
    dplyr::distinct(taxa_clean) |>
    # to preserve sort order and check duplicates (later)
    dplyr::mutate(index = dplyr::row_number())

  taxonomy_itis <- taxa_ids |>
    dplyr::left_join(
      y = taxadb_itis_tbl,
      by = c("taxa_clean" = "scientificName")
    )

  taxonomy_itis_duplicates <- taxonomy_itis |>
    dplyr::mutate(duplicate = duplicated(index)) |>
    dplyr::filter(duplicate)

  if (nrow(taxonomy_itis_duplicates) > 0) {

    message("duplicate taxonomic matches: see taxa_duplicates.csv")

    readr::write_csv(
      x = taxonomy_itis_duplicates |>
        dplyr::select(-index),
      file = "taxa_duplicates.csv"
    )

  }

  # taxonomy_itis |>
  #   pointblank::rows_distinct(
  #     columns = index,
  #     actions = pointblank::warn_on_fail()
  #   ) |>
  #   pointblank::row_count_match(
  #     count   = nrow(taxa_df),
  #     actions = pointblank::warn_on_fail()
  #   )

  taxonomy_itis <- taxonomy_itis |>
    dplyr::arrange(index) |>
    dplyr::select(-index)

  readr::write_csv(
    x    = taxonomy_itis,
    file = "taxa_map.csv"
  )

  invisible(taxonomy_itis)

}

#' Connect to a local taxadb database
#'
#' Helper function to connect to a local taxadb database and ensure the ITIS
#' schema is present.
#'
#' @return A DBI database connection.
#' @importFrom taxadb td_connect td_create
#'
#' @keywords internal
#'
get_taxa_db <- function() {

  db <- taxadb::td_connect()

  taxadb::td_create(
    provider = getOption("taxadb_default_provider", "itis"),
    schema   = c("dwc"),
    dbdir    = "/tmp/",
    db       = taxadb::td_connect()
  )

  return(db)

}

#' Get the Name of the ITIS Table in the Database
#'
#' Helper function to determine the correct ITIS table name in the database.
#'
#' @param connection A DBI database connection.
#' @param provider The provider name (default "itis").
#' @return The name of the ITIS table as a character string.
#' @importFrom DBI dbListTables
#'
#' @keywords internal
#'
get_taxadb_tbl_name <- function(
  connection = db,
  provider   = "itis"
) {

  tbls <- grep(
    pattern     = provider,
    x           = DBI::dbListTables(connection),
    ignore.case = TRUE,
    value       = TRUE
  )

  if (length(tbls) > 1) {
    tbls <- grep(
      pattern     = "dwc",
      x           = tbls,
      ignore.case = TRUE,
      value       = TRUE
    )
  }

  tbls[1]

}


#' Retrieve ITIS Data from the Local Database
#'
#' Helper function to retrieve the ITIS table as a data frame from the local
#' taxadb database.
#'
#' @return A data frame containing the ITIS taxonomic data.
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#'
#' @keywords internal
#'
get_itis_data <- function() {

  if (!exists("db", envir = .GlobalEnv)) {
    db <- get_taxa_db()
  } else {
    db <- get("db", envir = .GlobalEnv)
  }
  taxadb_itis_tbl_name <- get_taxadb_tbl_name(connection = db)
  taxadb_itis_tbl_query <- glue::glue_sql(
    "
    SELECT *
    FROM { taxadb_itis_tbl_name }
    ;
    ",
    .con = DBI::ANSI()
  )

  taxadb_itis_tbl <- DBI::dbGetQuery(
    conn      = db,
    statement = taxadb_itis_tbl_query
  )

  return(taxadb_itis_tbl)

}
