---
title: "create_spatialRaster"
author: "S. Earl"
date: "2019-10-14"
output: 
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

### overview

The capeml package provides the user considerable flexibility to process spatial
raster data and generate EML spatial raster metadata. Notably, the package
allows for processing an individual raster file (e.g., xxx.tiff) or a raster
file with supporting metadata (e.g., as xml or otherwise) in a zipped folder. In
the case of multiple files, all files in the parent directory where the raster
file is located are aggregated into a single compressed (zipped) file. In all
cases, the resulting entity is renamed with the project id + base file name +
md5sum + file extension (zip in the case when multiple files are aggregated).

The create_spatialRaster function takes numerous arguments, including:

- `rasterFile` (required) Quoted full path to raster file.
- `description` (required) Description of the raster.
- `epsgProjection` (required if emlProjection is not provided) EPSG numeric code
of raster's coordinate reference system
- `emlProjection` (required if epsgProjection is not provided) EML-compliant
refence to raster's coordinate reference system
- `rasterValueDescription` (required) Description of raster values
- `rasterValueUnits` Raster value units; these are optional but should be
provided if raster values are not categorical.
- `zipFiles` (optional, default = FALSE) Logical indicating whether spatial
raster entity should be constructed from a single raster file (FALSE, default)
or entire directory (TRUE)
- `baseURL` (optional) The base path of the web-accessible location of the data
file; the name of the resulting file will be passed to the base path to generate
a web-resolvable file path. The base path for CAP LTER data
(https://data.gios.asu.edu/datasets/cap/) is passed as the default.

Regardless of whether a single raster file or a zipped directory of related
files is created as the final entity, the raster file is read into the R
environment where select metadata are extracted using functions from the raster
package.

### projection

A projection is required for spatialRaster. This critical piece of metadata can
be provided by supplying the numeric epsg code of the projection (e.g., 4326 for
WGS 1984) or an EML-compliant projection name (e.g., WGS_1984_UTM_Zone_12N).
Ultimately, an EML-compliant projection name is required so the package will
attempt to match the appropriate EML-compliant projection name to epsg code if
epsg is provided but not an EML-compliant projection name. The package has a
limited number of epsg codes matched to EML-compliant projection names, mostly
those commonly used by CAP LTER investigators. The function will stop if a match
cannot be identified, and the user should contact the package administrator to
add the needed projection.

The user may supply an epsg code and an an EML-compliant projection name in
which case both will be represented in the metadata.

*Raster metadata example*

| metadata_entity | metadata_entity_description | metadata_value |
|:----------------|:----------------------------|:---------------|
| rasterName | name of the raster file (e.g. CAP_1985.img) | CAP_1985.img |
| rasterDescription | provide a description of the raster data | land use and land cover (LULC) map of the CAP LTER study based on 1985 Landsat imagery |
| rasterValueLabel | brief label applied to raster attribute (e.g. categorical raster cell value) | categorical raster cell value |
| rasterValueDescription | brief description of the raster attribute | categorical value of raster as it relates to a defined land use land cover (LULC) type |
| horizontalAccuracy | quantitative estimate expressed in the units of the coordinate system or as a text assessment | METADATA_NOT_PROVIDED |
| verticalAccuracy | quantitative estimate expressed in the units of the height or depth measurement system or as a text assessment | METADATA_NOT_PROVIDED |
| rasterOrigin | the corner of the grid where the first values for both the x and y axes begin in the file | Upper Left |
| verticals | maximum number of raster objects along the vertical (z) axis | 1 |
| cellGeometry | indication of whether the cell value is representative of a single point (matrix) within the cell or the entire cell (pixel) | pixel |



#### categorical raster values

If the raster values are categorical, generate a metadata file to catalog the unique raster value categories and their meaning. Additional metadata about the categories can be provided at the data set level.


*Raster categorical values metadata example*

|rasterValue | categoryName |
|:-----------|:-------------|
|1 | Water |
|2 | Asphalt/Road |
|3 | Concrete/Buildings |
|4 | Urban mixture |
|5 | Residential |
|6 | Residential (white rooftops) |
|7 | Active crop |
|8 | Inactive crop |
|9 | Cultivated vegetation |
|10 | Natural vegetation |
|11 | Soil/Desert |


### call the function

Call the create_spatialRaster function to generate the EML to describe the raster. Arguments include the quoted full or relative path to the raster file (or files), the quoted full or relative path and name of the raster metadata file, and the quoted full or relative path and name of the raster value categories (if needed). Output of the function yields EML that can be incorporated into the metadata for a data set.


```r
cap1985 <- create_spatialRaster('~/knb-lter-cap.650.1/CAP 30m Landsat Series Submit/',
                                '~/knb-lter-cap.650.1/rasterMetadataFiles/CAP1985_metadata.csv',
                                '~/knb-lter-cap.650.1/rasterMetadataFiles/landsat_factors.csv')
```

#### helper functions

create_spatialRaster calls two helper functions: get_emlProjection and zipRelatedFiles. 


##### get_emlProjection

EML requires projection data be included with raster metadata, and the projection details must be in a specific string form [valid EML CRS](https://knb.ecoinformatics.org/#external//emlparser/docs/eml-2.1.1/./eml-spatialReference.html). get_emlProjection attempts to identify the projection of a raster based on metadata inherent in the file and match it to the appropriate projection permissible by EML. If a match is not found, the string "METADATA_NOT_AVAILABLE" is returned in the resulting EML and must be edited manually.


##### zipRelatedFiles

Raster data typically consist of a single file (e.g., \*.img, \*.rrd) but are often accompanied by metadata (often in xml format) or other ancillary, supporting files. zipRelatedFiles attempts to identify any related files based on a common file name. For example, the raster `CAP_1985.img` is a raster file but has accompanying metadata/ancillary files titled `CAP_1985.aux.xml` and `CAP_1985.img.clr`. zipRelatedFiles will search the provided directory location for any files with the base name `CAP_1985`. All files with that base name will be transferred to a new directory and zipped into a single entity.


#### output

If all requisite metadata were provided and the get_emlProjection function successfully identified the raster projection, the create_spatialRaster function yields an EML-compliant object of type spatialRaster per the [ropensci EML package](https://github.com/ropensci/EML). The object name will include the `project id (identified in the working environment) + the base name of the raster + the md5sum of the raster + the file extension of the raster`. In the case where multiple files with a common base name were identified by zipRelatedFiles, the object will be of type `zip`.
