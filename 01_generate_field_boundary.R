library(mapedit)
library(mapview)
library(sf)

fb = drawFeatures()
mapview(fb)

st_write(fb, "field_boundary.gpkg")
