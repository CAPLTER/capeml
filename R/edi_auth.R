#' @title Log in to EDI with environment variables
#'
#' @description `edi_login()` establishes an EDI session for the current R
#' process when one is not already present. Credentials are read from the
#' `EDI_USER` and `EDI_PASS` environment variables.
#'
#' @details `capeml` calls this helper automatically before package functions
#' that use `EDIutils`. Call it directly in interactive workflows when you need
#' to use `EDIutils` yourself, for example with
#' `EDIutils::get_provenance_metadata()`.
#'
#' @param force
#'   (logical) Re-authenticate even if `EDI_TOKEN` or `AUTH_TOKEN` is already
#'   set in the current R session.
#'
#' @return Invisibly returns `TRUE` when an authenticated session is available.
#'
#' @export
edi_login <- function(force = FALSE) {

  edi_user <- Sys.getenv("EDI_USER", unset = "")
  edi_pass <- Sys.getenv("EDI_PASS", unset = "")

  if (!isTRUE(force) && has_edi_token()) {
    return(invisible(TRUE))
  }

  if (!nzchar(edi_user) || !nzchar(edi_pass)) {
    stop(
      "EDI credentials not found. Set EDI_USER and EDI_PASS in your .Renviron.",
      call. = FALSE
    )
  }

  EDIutils::login(
    userId = edi_user,
    userPass = edi_pass
  )

  invisible(TRUE)

}

has_edi_token <- function() {

  nzchar(Sys.getenv("EDI_TOKEN", unset = "")) ||
    nzchar(Sys.getenv("AUTH_TOKEN", unset = ""))

}