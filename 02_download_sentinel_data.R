library(sits)
library(mapview)
library(sf)
library(terra)

# these variables need to be set for the script to work
roi_file = "field_boundary.gpkg"
start = "2022-07-20"
end = "2022-07-22"

# 
roi = st_read(roi_file)

bnds = c(
  paste0(
    "B"
    , sprintf("%02.0f", 1:9)
  )
  , "B11"
  , "B12"
  , "B8A"
  , "CLOUD"
)

cube <- sits_cube(
  source = "AWS",
  collection = "SENTINEL-2-L2A",
  roi = roi, 
  bands = bnds,
  start_date = start,
  end_date = end
)

str(cube)

df_all = data.frame(
  paths = cube$file_info[[1]]$path
  , ccvr = cube$file_info[[1]]$cloud_cover
  , nms = cube$file_info[[1]]$band
  , date = cube$file_info[[1]]$date
  , res = cube$file_info[[1]]$xres
  , tresx = 10
  , tresy = 10
)
df_all = df_all[order(df_all$date), ]

df_ccvr_clean = df_all[df_all$ccvr <= 20, ]

dates = unique(df_ccvr_clean$date)
dirCreate = Vectorize(dir.create)

paths = file.path("sentinel_stacks", dates)

if (!dir.exists("sentinel_stacks")) {
  dir.create("sentinel_stacks")
}
dirCreate(paths)

bbx = st_geometry(roi) |>
  st_transform(cube$crs[1]) |>
  st_bbox()

df_lst = split(df_ccvr_clean, f = df_ccvr_clean$date)

for (i in seq(df_lst)) {
  
  # i = 1
  
  # current dataframe & date
  df = df_lst[[i]]
  date = unique(df$date)
  
  cat("\n[ <-- processing file", i, "-", as.character(date), "-", "of", length(df_lst), "--> ]\n")
  
  if (length(df$paths) > length(bnds)) {
    df = df[seq_along(bnds), ]
  }
  
  # save vsi paths for processing
  writeLines(
    df$paths
    , con = file.path(paths[i], "ifls.txt")
  )
  
  # temp files needed for processing
  vrt_fl = file.path(paths[i], "stack.vrt")
  stck_fl = file.path(paths[i], "stack.tif")
  
  # gdalbuildvrt with current opts
  vrt_cmd = paste(
    "gdalbuildvrt"
    , "-separate"
    , "-tr"
    , df$tresx[1]
    , df$tresy[1]
    , "-te"
    , bbx[1]
    , bbx[2]
    , bbx[3]
    , bbx[4]
    , "-r near"
    , "-input_file_list"
    , file.path(paths[i], "ifls.txt")
    , vrt_fl
  )
  
  # run gdalbuidvrt
  system(vrt_cmd)
  
  # couldn't figure out how to set band specific descriptions as terra does...
  # bnd_nms = paste("-mo", paste0("BAND_", 1:13, "=", bnds), collapse = " ")
  
  # translate to raster stack (.tif) with separate layers
  system(paste("gdal_translate", vrt_fl, stck_fl))
  
  # dirty hack to properly setting band info (i.e. orig. band name)
  rst = rast(stck_fl)
  names(rst) = df$nms
  
  writeRaster(
    rst
    , gsub(
      "stack.tif$", 
      paste0("stack_", date, ".tif")
      , stck_fl
    )
    , overwrite = TRUE
  )
  
  # delete temp file
  unlink(stck_fl)
}
