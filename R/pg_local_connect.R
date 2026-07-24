#' @title Connect to a local PostgreSQL database
#'
#' @description \code{pg_local_connect} creates a \code{DBI} connection to a
#' PostgreSQL database on \code{localhost} using the modern
#' \code{RPostgres} backend.
#'
#' @details Database selection precedence is:
#' \enumerate{
#'   \item explicit \code{db} argument,
#'   \item R option \code{pg_local_db},
#'   \item environment variable \code{PG_LOCAL_DB},
#'   \item default \code{"caplter"}.
#' }
#'
#' User credential selection precedence is:
#' \enumerate{
#'   \item explicit \code{user} or \code{password} argument,
#'   \item environment variables \code{DB_USER} and \code{POSTGRES}.
#' }
#'
#' This function is intended as a convenience helper for local development and
#' data exploration workflows that need a PostgreSQL connection while keeping
#' credentials outside the codebase.
#'
#' @note Set credentials in \code{~/.Renviron} to avoid hardcoding them in
#' scripts, for example:
#' \preformatted{DB_USER=your_username
#' POSTGRES=your_password
#' PG_LOCAL_DB=caplter}
#'
#' @param db
#' (character) Database name. If \code{NULL}, missing, or empty, the function
#' falls back to \code{getOption("pg_local_db")}, then \code{PG_LOCAL_DB},
#' then \code{"caplter"}.
#' @param user
#' (character) PostgreSQL username. If \code{NULL}, missing, or empty, the
#' function attempts to read \code{DB_USER} from the environment.
#' @param password
#' (character) PostgreSQL password. If \code{NULL}, missing, or empty, the
#' function attempts to read \code{POSTGRES} from the environment.
#' @param host
#' (character) PostgreSQL host name. Defaults to \code{"localhost"}.
#' @param port
#' (integer) PostgreSQL port. Defaults to \code{5432}.
#'
#' @return A \code{DBIConnection} object.
#'
#' @examples
#' \dontrun{
#'
#' # use DB_USER / DB_PASSWORD from ~/.Renviron
#' pg <- pg_local_connect()
#'
#' # change the default database for the current session
#' options(pg_local_db = "dev")
#' pg_dev <- pg_local_connect()
#'
#' # override user and password explicitly
#' pg_override <- pg_local_connect(
#'   db       = "caplter",
#'   user     = "my_user",
#'   password = "my_password"
#' )
#'
#' }
#'
#' @export
pg_local_connect <- function(
  db       = NULL,
  user     = NULL,
  password = NULL,
  host     = "localhost",
  port     = 5432L
  ) {

  db_name <- db
  if (is.null(db_name) || !nzchar(trimws(db_name))) {
    db_name <- getOption("pg_local_db", "")
  }
  if (!nzchar(trimws(db_name))) {
    db_name <- Sys.getenv("PG_LOCAL_DB", unset = "")
  }
  if (!nzchar(trimws(db_name))) {
    db_name <- "caplter"
  }

  db_user <- user
  if (is.null(db_user) || !nzchar(trimws(db_user))) {
    db_user <- Sys.getenv("DB_USER", unset = "")
  }
  if (!nzchar(trimws(db_user))) {
    stop(
      "database user not supplied. Set DB_USER in ~/.Renviron or pass `user`.",
      call. = FALSE
    )
  }

  db_password <- password
  if (is.null(db_password) || !nzchar(trimws(db_password))) {
    db_password <- Sys.getenv("POSTGRES", unset = "")
  }
  if (!nzchar(trimws(db_password))) {
    stop(
      "database password not supplied. Set POSTGRES in ~/.Renviron or pass `password`.",
      call. = FALSE
    )
  }

  DBI::dbConnect(
    drv      = RPostgres::Postgres(),
    dbname   = db_name,
    host     = host,
    port     = as.integer(port),
    user     = db_user,
    password = db_password
  )

}
