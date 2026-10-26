# zip related files (deprecated)

Zip files in a common directory that share the same base name (i.e.,
file name sans path and extension). Zipped file includes all relevant
files zipped into a single, compressed file named as: package number +
basefile name + md5sum + extension (zip).

## Usage

``` r
zipRelatedFiles(pathToFile, targetFile)
```

## Arguments

- pathToFile:

  quoted name of the directory where the target file is located
  directory

- targetFile:

  quoted name of target data file to which other file names in the
  prescribed directory will be evalated directory

## Value

the zipped file name for inclusion in the EML documentation - not
relevant when run independently

## Details

zipRelatedFiles is a helper function designed primarily to assist the
creation of EML spatial data objects. A spatial data entity often
consists of more than one file (e.g., shapefiles; raster (\*.img,
\*.xml)) that are incorporated into a single zipped file for upload to
PASTA. This function zips related files as determined by the same base
file name (e.g., CAP_1985.img and CAP_1985.img.aux.xml share the same
base name and would be zipped). In keeping with the CAP LTER workflow
for generating EML metadata, the resulting compressed file is named
package number + basefile name + md5sum + extension (zip). Though
intended primarily as a helper function, zipRelatedFiles can be run
independently.

## Note

zipRelatedFiles is specific to UNIX operating environments.

## Examples

``` r
if (FALSE) { # \dontrun{
zipRelatedFiles(
  pathToFile = "~/folder/",
  targetFile = "CAP_1985.img")
} # }
```
