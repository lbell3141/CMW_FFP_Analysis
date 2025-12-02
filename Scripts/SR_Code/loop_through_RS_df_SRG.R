library(terra)

#==============================
# Load CHM and Cover rasters
#==============================
# chm <- rast("./Data/RussHomework/SRG_RS/CHM.tif")
# chm <- terra::ifel(chm > 0.3, chm, NA)
# 
# cover <- rast("./Data/RussHomework/SRG_RS/Cover.tif")

classified <- rast("./Data/RussHomework/SRG_RS/testrast.tif")
#==============================
# FFP files per month
#==============================
ffp_files <- list(
  May   = "./Data/RussHomework/SRG_2024_ffps/may_calcd_ffp_list.rds",
  Jun   = "./Data/RussHomework/SRG_2024_ffps/jun_calcd_ffp_list.rds",
  Jul   = "./Data/RussHomework/SRG_2024_ffps/jul_calcd_ffp_list.rds",
  Aug   = "./Data/RussHomework/SRG_2024_ffps/aug_calcd_ffp_list.rds",
  Sep   = "./Data/RussHomework/SRG_2024_ffps/sep_calcd_ffp_list.rds"
)

#==============================
# Site coordinates
#==============================
site_lat <- 31.7894
site_lon <- -110.8277

#==============================
# Function: FFP contours to mask (robust)
#==============================
ffp_contours_to_mask <- function(xr_list, yr_list, rast, site_center_lat, site_center_lon) {
  
  # 1️⃣ Convert points to numeric matrices
  coord_pair_list <- lapply(seq_along(xr_list), function(i) {
    x_vec <- as.numeric(unlist(xr_list[[i]]))
    y_vec <- as.numeric(unlist(yr_list[[i]]))
    idx <- !is.na(x_vec) & !is.na(y_vec)
    mat <- cbind(x_vec[idx], y_vec[idx])
    if(nrow(mat) == 0) return(NULL)
    mat
  })
  
  # Remove empty contours
  coord_pair_list <- coord_pair_list[!sapply(coord_pair_list, is.null)]
  if(length(coord_pair_list) == 0) return(0)  # all empty
  
  # 2️⃣ Convert to lat/lon
  latlon_list <- lapply(coord_pair_list, function(mat) {
    lat <- (mat[,2]/111111) + site_center_lat
    lon <- (mat[,1]/(111111*cos(site_center_lat*pi/180))) + site_center_lon
    cbind(lon, lat)
  })
  
  # 3️⃣ Create SpatVector lines in WGS84 and reproject to raster CRS
  lines_list <- lapply(latlon_list, function(mat) {
    line <- terra::vect(mat, type="lines", crs="EPSG:4326")
    terra::project(line, terra::crs(rast))
  })
  
  # 4️⃣ Mask raster for each contour
  masked_rast <- lapply(lines_list, function(line) terra::mask(rast, line))
  
  # 5️⃣ Subtract inner contours to get separate areas
  dif_con_list <- list()
  for(i in seq_along(masked_rast)) {
    if(i == 1) {
      dif_con_list[[i]] <- masked_rast[[i]]
    } else {
      dif_con_list[[i]] <- terra::mask(masked_rast[[i]], masked_rast[[i-1]], inverse=TRUE)
    }
  }
  
  # 6️⃣ Compute weighted mean (handle all-NA rasters)
  rap_vals <- sapply(dif_con_list, function(r) {
    v <- terra::values(r)
    if(all(is.na(v))) return(0)
    mean(v, na.rm=TRUE)
  })
  
  weights <- rep(1/length(rap_vals), length(rap_vals))
  veg_cover <- sum(rap_vals * weights)
  
  return(veg_cover)
}

#==============================
# Function: Process one month
#==============================
process_ffp_month <- function(ffp_file, chm, cover, site_lat, site_lon) {
  
  ffp_list <- readRDS(ffp_file)
  
  # Split into x/y
  x_list <- lapply(ffp_list, function(ffp) ffp$xr)
  y_list <- lapply(ffp_list, function(ffp) ffp$yr)
  
  # Compute veg_cover from CHM
  chm_cover <- mapply(
    ffp_contours_to_mask,
    x_list,
    y_list,
    MoreArgs = list(rast = chm, site_center_lat = site_lat, site_center_lon = site_lon)
  )
  
  # Compute veg_cover from Cover raster
  cover_vals <- mapply(
    ffp_contours_to_mask,
    x_list,
    y_list,
    MoreArgs = list(rast = cover, site_center_lat = site_lat, site_center_lon = site_lon)
  )
  
  # Combine into dataframe
  deg_int <- seq(20, 360, by = 20)
  data.frame(
    direction = deg_int,
    veg_cover_chm = unlist(chm_cover),
    veg_cover_cover = unlist(cover_vals)
  )
}

# Apply to all months
monthly_df_list <- lapply(names(ffp_files), function(month) {
  df <- process_ffp_month(ffp_files[[month]], chm, cover, site_lat, site_lon)
  df$month <- month
  df
})

# Combine into one dataframe
all_months_df <- do.call(rbind, monthly_df_list)

# Check
head(all_months_df)
summary(all_months_df)
