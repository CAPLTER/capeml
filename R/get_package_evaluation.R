#' @title evaluate data package with EDI API
#'
#' @description \code{get_package_evaluation} will use the generated eml.xml
#' and uploaded data to check the congruence of a data package.
#'
#' @note This is a full evaluation of the data package, including checking
#' supplied metadata against provided data, as opposed to, for example, just
#' checking that the schema is valid. As such, the data files must be available
#' to EDI for proper evaluation.
#'
#' @note \code{get_package_evaluation} will look for a config.yaml file to
#' estalish the package scope and identifier. Alternatively, these parameters
#' can be provided by the user but, in that case, both parameters must be
#' provided.
#'
#' @param identifier
#'  (integer) Unquoted package identifier integer
#' @param scope
#'  (character) Quoted package scope (e.g., "knb-lter-cap", "edi")
#' @param full_report
#'  (logical) Logical indicating whether to return a summary (default) or
#'  detailed evaluation report.
#'
#' @import EDIutils
#'
#' @return summary or detailed evaluation report
#'
#' @export
#'
get_package_evaluation <- function(
  identifier  = NULL,
  scope       = NULL,
  full_report = FALSE
  ) {

  if (is.null(identifier)) || is.null(scope) {

    if (!file.exists("config.yaml")) {

      stop("config.yaml not found")

    }

    package_configs <- capeml::read_package_configuration()
    identifier      <- package_configs$identifier
    scope           <- package_configs$scope

  }

  version <- capeml::get_next_version(
    provided_scope      = scope,
    provided_identifier = identifier

  )

  evaluation <- EDIutils::evaluate_data_package(
    eml         = paste(scope, identifier, version, "xml", sep = "."),
    useChecksum = FALSE,
    env         = "staging"
  )

  Sys.sleep(8)

  eval_status <- EDIutils::check_status_evaluate(
    transaction = evaluation,
    env         = "staging"

  )

  if (eval_status) {

    if (full_report == FALSE) {

      # evaluation: summary

      EDIutils::read_evaluate_report_summary(
        transaction = evaluation,
        env         = "staging"
      )

    } else {

      # evaluation: detailed

      EDIutils::read_evaluate_report(
        transaction = evaluation,
        env         = "staging"
      )

    }

  } 

}
