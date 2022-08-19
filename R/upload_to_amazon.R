#' @title upload_to_amazon
#'
#' @description The functions data_to_amz and eml_to_amz are convenience
#' functions to facilitate uploading data and metadata (xml) files,
#' respectively, to a bucket in the AWS S3 ecosystem.
#'
#' @note Functionality relies on setting AWS credentials in the environment.
#' @note These functions default to settings appropriate for CAP LTER data,
#' other users should set bucket and path variables as appropriate.
#'
#' @param file_to_upload
#' (character) The quoted name and path (if relevant) of the file to be
#' uploaded.
#' @param data_path
#' (character) The quoted name to the directory in the AWS environment to store
#' data files.
#' @param metadata_path
#' (character) The quoted name to the directory in the AWS environment to store
#' metadata files.
#' @param aws_bucket
#' (character) The AWS bucket to which data and/or metadata should be
#' contributed.
#'
#' @importFrom aws.s3 put_object
#'
#' @examples
#' \dontrun{
#'
# data_to_amz("650_CAP_1985_0c95b18e82df5eb0302a46e5967bb1e1.zip")
#'
# eml_to_amz("~/localRepos/cap-data/cap-data-eml/knb-lter-cap.650.1.xml")
#'
#' }

#' @export
data_to_amz <- function(
  file_to_upload,
  data_path     = "/datasets/cap/",
  metadata_path = "/metadata/",
  aws_bucket    = "gios-data"
  ) {

  aws.s3::put_object(
    file   = file_to_upload,
    object = paste0(data_path, basename(file_to_upload)),
    bucket = aws_bucket
  )

}

#' @rdname data_to_amz
#' @export
eml_to_amz <- function(file_to_upload) {

  aws.s3::put_object(
    file   = file_to_upload,
    object = paste0(metadata_path, basename(file_to_upload)),
    bucket = aws_bucket
  )

}
