library(mapedit)
library(mapview)
library(sf)

fb = drawFeatures()
mapview(fb)

if (!dir.exists("field_boundary")) {
  dir.create("field_boundary")
}

st_write(fb, "field_boundary/field_boundary.gpkg")
