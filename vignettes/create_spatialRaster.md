# create_spatialRaster
S. Earl  
`r Sys.Date()`  

## detail raster metdata

For each raster, generate a separate metadata file to detail critical information about the raster. Note that the raster file name should be provided as unquoted text, and the file (or files) should be in the directory specified when calling the create_spatialRaster function.





metadata_entity          metadata_entity_description                                                                                                    metadata_value                                                                         
-----------------------  -----------------------------------------------------------------------------------------------------------------------------  ---------------------------------------------------------------------------------------
rasterName               name of the raster file (e.g. CAP_1985.img)                                                                                    CAP_1985.img                                                                           
rasterDescription        provide a description of the raster data                                                                                       land use and land cover (LULC) map of the CAP LTER study based on 1985 Landsat imagery 
rasterValueLabel         brief label applied to raster attribute (e.g. categorical raster cell value)                                                   categorical raster cell value                                                          
rasterValueDescription   brief description of the raster attribute                                                                                      categorical value of raster as it relates to a defined land use land cover (LULC) type 
horizontalAccuracy       quantitative estimate expressed in the units of the coordinate system or as a text assessment                                  METADATA_NOT_PROVIDED                                                                  
verticalAccuracy         quantitative estimate expressed in the units of the height or depth measurement system or as a text assessment                 METADATA_NOT_PROVIDED                                                                  
rasterOrigin             the corner of the grid where the first values for both the x and y axes begin in the file                                      Upper Left                                                                             
verticals                maximum number of raster objects along the vertical (z) axis                                                                   1                                                                                      
cellGeometry             indication of whether the cell value is representative of a single point (matrix) within the cell or the entire cell (pixel)   pixel                                                                                  



### categorical raster values

If the raster values are categorical, generate a metadata file to catalog the unique raster value categories and their meaning. Additional metadata about the categories can be provided at the data set level.




 rasterValue  categoryName                 
------------  -----------------------------
           1  Water                        
           2  Asphalt/Road                 
           3  Concrete/Buildings           
           4  Urban mixture                
           5  Residential                  
           6  Residential (white rooftops) 
           7  Active crop                  
           8  Inactive crop                
           9  Cultivated vegetation        
          10  Natural vegetation           
          11  Soil/Desert                  


## call the function

Call the create_spatialRaster function to generate the EML to describe the raster. Arguments include the quoted path to the raster file (or files), the quoted path and name of the raster metadata file, and the quoted path and name of the raster value categories (if needed). Output of the function yields EML that can be incorporated into the metadata for a data set.


```r
cap1985 <- create_spatialRaster('~/knb-lter-cap.650.1/CAP 30m Landsat Series Submit/',
                                '~/knb-lter-cap.650.1/rasterMetadataFiles/CAP1985_metadata.csv',
                                '~/knb-lter-cap.650.1/rasterMetadataFiles/landsat_factors.csv')
```

### helper functions

create_spatialRaster calls two helper functions: get_emlProjection and zipRelatedFiles. 


#### get_emlProjection

EML requires projection data be included with raster metadata, and the projection details must be in a specific string form [valid EML CRS](https://knb.ecoinformatics.org/#external//emlparser/docs/eml-2.1.1/./eml-spatialReference.html). get_emlProjection attempts to identify the projection of a raster based on metadata inherent in the file and match it to the appropriate projection permissible by EML. If a match is not found, the string "METADATA_NOT_AVAILABLE" is returned in the resulting EML and must be edited manually.


#### zipRelatedFiles

Raster data typically consist of a single file (e.g., \*.img, \*.rrd) but are often accompanied by metadata (often in xml format) or other ancillary, supporting files. zipRelatedFiles attempts to identify any related files based on a common file name. For example, the raster `CAP_1985.img` is a raster file but has accompanying metadata/ancillary files titled `CAP_1985.aux.xml` and `CAP_1985.img.clr`. zipRelatedFiles will search the provided directory location for any files with the base name `CAP_1985`. All files with that base name will be transferred to a new directory and zipped into a single entity.


### output

If all requisite metadata were provided and the get_emlProjection function successfully identified the raster project, the create_spatialRaster function yields an EML-compliant  object of type spatialRaster per the [ropensci EML package](https://github.com/ropensci/EML). The object name will include the `project id (identified in the working environment) + the base name of the raster + the md5sum of the raster + the file extension of the raster`. In the case where multiple files with a common base name were identified by zipRelatedFiles, the object will be of type `zip`.
