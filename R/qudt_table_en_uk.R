#' @title qudt_table: reference table of QUDT units in english (UK) 
#'
#' @description \code{qudt_table} contains units-of-measure details outlined by
#' the Quantities, Units, Dimensions and dataTypes organization
#' [QUDT.org](https://qudt.org/). The table provides a resource for documenting
#' units of type QUDT in EML metadata. This is a subset of the qudt_table that
#' features only english (UK) language entries.
#' 
#' @note The unit \code{description} currently is commented out in the query
#' owing to poorly formatted text and character issues. This is not needed in
#' the current \code{capeml} workflow.
#' @note Even when \code{description} is included in the query, be cautious of
#' filtering by datatype (e.g., Latex) as even though \code{description} is
#' included as an optional return the filter will omit entries without a
#' datatype.
#' @note \code{ucumCode} currently is commented out in the query as including
#' this variable results in many duplicate entries that vary only by the
#' \code{ucumCode} value. \code{ucumCode} is not part of the \code{capeml}
#' workflow; consider aggregating these values into an array if needed in the
#' future.
#' @note Return is restricted to entries noted as being documented in english
#' (US).
#'
#' @format A dataframe; rows will vary as QUDT adds units and columns may vary
#' if the query is altered.
#' \describe{
#'  \item{unit}{unit URI}
#'  \item{label}{short description of the unit}
#'  \item{hasDimensionVector}{dimension vector of unit}
#'  \item{conversionMultiplier}{multiplier to SI}
#'  \item{lang}{language format (US or British english)}
#'  \item{name}{unit identifier}
#' }
#'
#' @source \url{https://raw.githubusercontent.com/qudt/qudt-public-repo/master/vocab/unit/VOCAB_QUDT-UNITS-ALL-v2.1.ttl}
"qudt_table_en_uk"