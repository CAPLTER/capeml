## ---- echo=FALSE---------------------------------------------------------
load(file = "CAP1985_metadata.rda")

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(CAP1985_metadata)

## ---- echo=FALSE---------------------------------------------------------
load(file = "CAP1985_factors.rda")

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(CAP1985_factors)

## ---- eval=FALSE---------------------------------------------------------
#  cap1985 <- create_spatialRaster('~/knb-lter-cap.650.1/CAP 30m Landsat Series Submit/',
#                                  '~/knb-lter-cap.650.1/rasterMetadataFiles/CAP1985_metadata.csv',
#                                  '~/knb-lter-cap.650.1/rasterMetadataFiles/landsat_factors.csv')

