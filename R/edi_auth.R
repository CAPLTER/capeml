#' @title Configure EDI API key authentication
#'
#' @description `edi_login()` ensures that an EDI API key is available to the
#' current R process. The key is read from the `EDI_API_KEY` environment
#' variable.
#'
#' @details `capeml` calls this helper automatically before package functions
#' that use `EDIutils`. Call it directly in interactive workflows when you need
#' to use `EDIutils` yourself, for example with
#' `EDIutils::get_provenance_metadata()`.
#'
#' @param force
#'   (logical) Register the API key with `EDIutils::login()` even when
#'   `EDI_API_KEY` is already set in the current R session.
#'
#' @return Invisibly returns `TRUE` when API key authentication is configured.
#'
#' @export
edi_login <- function(force = FALSE) {

  edi_api_key <- Sys.getenv("EDI_API_KEY", unset = "")

  if (!isTRUE(force) && nzchar(edi_api_key)) {
    return(invisible(TRUE))
  }

  if (!nzchar(edi_api_key)) {
    stop(
      "EDI API key not found. Set EDI_API_KEY in your .Renviron.",
      call. = FALSE
    )
  }

  EDIutils::login(
    key = edi_api_key
  )

  invisible(TRUE)

}