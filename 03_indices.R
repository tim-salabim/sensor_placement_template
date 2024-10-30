library(terra)
library(sf)
library(mapview)

fls = list.files(
  "sentinel_stacks"
  , pattern = "\\d{4}-\\d{2}-\\d{2}"
  , full.names = TRUE
) |>
  list.files(
  pattern = "^stack_\\d{4}-\\d{2}-\\d{2}.tif$"
  , full.names = TRUE
)

nms = basename(fls) |> tools::file_path_sans_ext()

rst_lst = lapply(fls, rast)
names(rst_lst) = nms

ndvi = function(x) {
  out = (x["B08"] - x["B04"]) / (x["B08"] + x["B04"])
  names(out) = "ndvi"
  return(out)
}


ndwi = function(x) {
  out = (x["B08"] - x["B12"]) / (x["B08"] + x["B12"])
  names(out) = "ndwi"
  return(out)
}

nsds = function(x) {
  out = (x["B11"] - x["B12"]) / (x["B11"] + x["B12"])
  names(out) = "nsds"
  return(out)
}

ndmi = function(x) {
  out = (x["B8A"] - x["B11"]) / (x["B8A"] + x["B11"])
  names(out) = "ndmi"
  return(out)
}

msi = function(x) {
  out = 
    x["B11"] / x["B08"]
  names(out) = "msi"
  return(out)
}

ndvi_stack = rast(lapply(rst_lst, ndvi))
ndwi_stack = rast(lapply(rst_lst, ndwi))
nsds_stack = rast(lapply(rst_lst, nsds))
ndmi_stack = rast(lapply(rst_lst, ndmi))
msi_stack = rast(lapply(rst_lst, msi))

lst = list(
  # ndvi_mean = mean(ndvi_stack)
  # , ndvi_var = app(ndvi_stack, "var")
  # , ndwi_mean = mean(ndwi_stack)
  # , ndwi_var = app(ndwi_stack, "var")
  nsds_mean = mean(nsds_stack)
  , nsds_var = app(nsds_stack, "var")
  , msi_mean = mean(msi_stack)
  , msi_var = app(msi_stack, "var")
  # , ndmi_mean = mean(ndmi_stack)
  # , ndmi_var = app(ndmi_stack, "var")
)

mapview(lst$msi_mean)

field_boundary = st_read("field_boundary.gpkg")
field_boundary = st_transform(field_boundary, st_crs(lst[[1]]))
field_boundary_buffered = st_buffer(field_boundary, -50)

mapview(lst$msi_mean) + field_boundary_buffered


clustered_rst_lst = lapply(
  seq(lst)
  , \(i) {
    
    # i = 1
    
    rst = lst[[i]]
    rst_masked = mask(rst, field_boundary_buffered)
    
    vals = as.vector(values(rst_masked))
    
    id = which(!is.na(vals))
    vals_nona = vals[id]
    
    set.seed(2048)
    km = kmeans(vals_nona, centers = 3, iter.max = 100)
    srt = order(km$centers)
    km$cluster = match(1:3, srt)[km$cluster]
    km$centers = matrix(km$centers[srt], nrow = 3, dimnames = list(1:3))
    
    lab = rep(NA, length(vals))
    lab[id] = km$cluster
    
    values(rst_masked) = lab
    
    rst_out = focal(rst_masked, w = 3, fun = "modal")
    rst_out = as.factor(rst_out)
    return(
      list(
        rst = rst_out
        , clstr = km
      )
    )
  }
)
names(clustered_rst_lst) = names(lst)

### TODO: examine kmeans stats and plots

min_area = 101

pols = lapply(
  seq(clustered_rst_lst)
  , \(i) {
    pol = st_as_sf(as.polygons(clustered_rst_lst[[i]]$rst))
    pol = st_cast(pol, "MULTIPOLYGON") |> st_cast("POLYGON")
    
    pol = pol[as.vector(st_area(pol)) >= min_area, ]
    # print(st_geometry_type(pol))
    pol$index = names(lst)[i]
    return(pol[c(3, 1, 2)])
  }
)
names(pols) = names(lst)

foc_mod_mean = 2
foc_mod_var = 2
is_var = grepl("var", names(lst))

pols_mean = lapply(
  seq(pols)
  , \(i) {
    x = pols[[i]]
    
    if (is_var[i]) {
      return(subset(x, x$focal_modal == foc_mod_var))
    }
    return(subset(x, x$focal_modal == foc_mod_mean))
  }
)
names(pols_mean) = names(lst)

isl = Reduce(sf::st_intersection, x = lapply(pols_mean, st_geometry))
if (any(st_dimension(isl) > 1)) {
  pl = st_collection_extract(isl, "POLYGON") |> st_cast("POLYGON")
  pl = pl[as.vector(st_area(pl)) >= min_area, ]
} else {
  stop("no polygonal intersection found")
}

mapview(clustered_rst_lst$nsds_mean$rst) + 
  mapview(pl)

pl_smooth = smoothr::smooth(
  pl
  , method = "ksmooth"
  , bandwidth = 25
)

mapview(clustered_rst_lst$msi_var$rst) + 
  mapview(pl_smooth)

