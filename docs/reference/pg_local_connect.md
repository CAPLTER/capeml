# Connect to a local PostgreSQL database

`pg_local_connect` creates a `DBI` connection to a PostgreSQL database
on `localhost` using the modern `RPostgres` backend.

## Usage

``` r
pg_local_connect(
  db = NULL,
  user = NULL,
  password = NULL,
  host = "localhost",
  port = 5432L
)
```

## Arguments

- db:

  (character) Database name. If `NULL`, missing, or empty, the function
  falls back to `getOption("pg_local_db")`, then `PG_LOCAL_DB`, then
  `"caplter"`.

- user:

  (character) PostgreSQL username. If `NULL`, missing, or empty, the
  function attempts to read `DB_USER` from the environment.

- password:

  (character) PostgreSQL password. If `NULL`, missing, or empty, the
  function attempts to read `DB_PASSWORD` from the environment.

- host:

  (character) PostgreSQL host name. Defaults to `"localhost"`.

- port:

  (integer) PostgreSQL port. Defaults to `5432`.

## Value

A `DBIConnection` object.

## Details

Database selection precedence is:

1.  explicit `db` argument,

2.  R option `pg_local_db`,

3.  environment variable `PG_LOCAL_DB`,

4.  default `"caplter"`.

User credential selection precedence is:

1.  explicit `user` or `password` argument,

2.  environment variables `DB_USER` and `DB_PASSWORD`.

This function is intended as a convenience helper for local development
and data exploration workflows that need a PostgreSQL connection while
keeping credentials outside the codebase.

## Note

Set credentials in `~/.Renviron` to avoid hardcoding them in scripts,
for example:

    DB_USER=your_username
    DB_PASSWORD=your_password
    PG_LOCAL_DB=caplter

## Examples

``` r
if (FALSE) { # \dontrun{

# use DB_USER / DB_PASSWORD from ~/.Renviron
pg <- pg_local_connect()

# change the default database for the current session
options(pg_local_db = "dev")
pg_dev <- pg_local_connect()

# override user and password explicitly
pg_override <- pg_local_connect(
  db       = "caplter",
  user     = "my_user",
  password = "my_password"
)

} # }
```
