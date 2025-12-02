#SRG cover
#===============================================================================
#load ffps and RAP data
ffp_list <- readRDS("./Data/RussHomework/SRG_2024_ffps/jun_calcd_ffp_list.rds")

#==============================
# Libraries
#==============================
library(terra)

#==============================
# Function: FFP contours to mask (terra-compatible)
#==============================
ffp_contours_to_mask <- function(xr_list, yr_list, rast, site_center_lat, site_center_lon) {
  
  # 1️⃣ Convert points to numeric matrices
  coord_pair_list <- lapply(seq_along(xr_list), function(i) {
    df <- data.frame(x = as.numeric(xr_list[[i]]), y = as.numeric(yr_list[[i]]))
    df <- na.omit(df)
    as.matrix(df)
  })
  
  # 2️⃣ Convert to lat/lon using site center (WGS84)
  latlon_list <- lapply(coord_pair_list, function(mat) {
    lat <- (mat[, 2] / 111111) + site_center_lat
    lon <- (mat[, 1] / (111111 * cos(site_center_lat * pi / 180))) + site_center_lon
    cbind(lon, lat)
  })
  
  # 3️⃣ Create SpatVector lines in WGS84
  lines_list <- lapply(latlon_list, function(mat) terra::vect(mat, type = "lines", crs = "EPSG:4326"))
  
  # 4️⃣ Reproject lines to CHM CRS
  lines_list <- lapply(lines_list, function(line) terra::project(line, terra::crs(rast)))
  
  # 5️⃣ Select contour lines (adjust indices depending on FFP output)
  part_con_list <- list(lines_list[[4]], lines_list[[7]], lines_list[[10]])
  
  # 6️⃣ Mask CHM with contours
  masked_rast <- lapply(part_con_list, function(line) terra::mask(rast, line))
  
  # 7️⃣ Subtract inner contours to get separate contour areas
  dif_con_list <- list(
    masked_rast[[1]],
    terra::mask(masked_rast[[2]], masked_rast[[1]], inverse = TRUE),
    terra::mask(masked_rast[[3]], masked_rast[[2]], inverse = TRUE)
  )
  
  # 8️⃣ Compute mean CHM values
  rap_vals <- sapply(dif_con_list, function(r) mean(terra::values(r), na.rm = TRUE))
  
  # 9️⃣ Weighted sum (adjust weights as needed)
  veg_cover <- 0.3 * sum(rap_vals[1]) +
    0.3 * sum(rap_vals[2]) +
    0.3 * sum(rap_vals[3])
  
  return(veg_cover)
}

#==============================
# Apply function to all FFPs
#==============================

chm <- rast("./Data/RussHomework/SRG_RS/CHM.tif")
chm <- terra::ifel(chm > 0.3, chm, NA)

cover <- rast("./Data/RussHomework/SRG_RS/Cover.tif")

#==============================
# Load FFP list (contours)
#==============================
ffp_list <- readRDS("./Data/RussHomework/SRG_2024_ffps/may_calcd_ffp_list.rds")
ffp_list <- readRDS("./Data/RussHomework/SRG_2024_ffps/jun_calcd_ffp_list.rds")
ffp_list <- readRDS("./Data/RussHomework/SRG_2024_ffps/jul_calcd_ffp_list.rds")
ffp_list <- readRDS("./Data/RussHomework/SRG_2024_ffps/aug_calcd_ffp_list.rds")
ffp_list <- readRDS("./Data/RussHomework/SRG_2024_ffps/sep_calcd_ffp_list.rds")

# Split FFP into x and y coordinate lists
x_list <- lapply(ffp_list, function(ffp) ffp$xr)
y_list <- lapply(ffp_list, function(ffp) ffp$yr)


site_lat <- 31.7894   # New site latitude
site_lon <- -110.8277 # New site longitude

rap_ffp_list <- mapply(
  ffp_contours_to_mask,
  x_list,
  y_list,
  MoreArgs = list(rast = chm, site_center_lat = site_lat, site_center_lon = site_lon)
)

#==============================
# Convert results to dataframe
#==============================
deg_int <- seq(20, 360, by = 20)
deg_int_real <- deg_int

rap_ffp_df <- data.frame(
  direction = deg_int_real,
  veg_cover = unlist(rap_ffp_list)
)

may <- rap_ffp_df
