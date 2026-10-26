# upload_to_amazon

The functions data_to_amz and eml_to_amz are convenience functions to
facilitate uploading data and metadata (xml) files, respectively, to a
bucket in the AWS S3 ecosystem.

## Usage

``` r
data_to_amz(
  file_to_upload,
  data_path = "/datasets/cap/",
  aws_bucket = "gios-data"
)

eml_to_amz(
  file_to_upload,
  metadata_path = "/metadata/",
  aws_bucket = "gios-data"
)
```

## Arguments

- file_to_upload:

  (character) The quoted name and path (if relevant) of the file to be
  uploaded.

- data_path:

  (character) The quoted name to the directory in the AWS environment to
  store data files.

- aws_bucket:

  (character) The AWS bucket to which data and/or metadata should be
  contributed.

- metadata_path:

  (character) The quoted name to the directory in the AWS environment to
  store metadata files.

## Note

Functionality relies on setting AWS credentials in the environment.

These functions default to settings appropriate for CAP LTER data, other
users should set bucket and path variables as appropriate.

## Examples

``` r
if (FALSE) { # \dontrun{

data_to_amz("650_CAP_1985_0c95b18e82df5eb0302a46e5967bb1e1.zip")

eml_to_amz("~/localRepos/cap-data/cap-data-eml/knb-lter-cap.650.1.xml")

} # }
```
