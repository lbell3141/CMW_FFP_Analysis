# =====================================================================
# Clean & Optimized Half-Hour Footprint Calculation Script
# =====================================================================

library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(furrr)

# Source footprint function
source("./Scripts/kljun_code/calc_footprint_FFP_climatology.R")

# =====================================================================
# 1. Load flux tower half-hour climate data
# =====================================================================
SRGtowerdat <- read.csv(
  "./Data/RussHomework/GapfilledPartitionedFluxes_US-SRG_HH_ALL/GapfilledPartitionedFluxes_US-SRG_HH_202312312330_202412312330.csv",
  na.strings = "-9999"
) %>%
  mutate(
    TIMESTAMP_END = ymd_hm(TIMESTAMP_END),
    yyyy = year(TIMESTAMP_END),
    mm = month(TIMESTAMP_END),
    day = day(TIMESTAMP_END),
    HH = hour(TIMESTAMP_END),
    MM = minute(TIMESTAMP_END)
  ) %>%
 # filter(WS_1_1_1 >= 0, HH %in% 8:17, mm %in% 7:9)
filter(WS_1_1_1 >= 0, HH %in% 8:17, mm == 8)

# =====================================================================
# 2. Load FFP input meteorology and merge
# =====================================================================
SRGffpdat <- read.csv("./Data/RussHomework/Input2dFootprint_SRG2024.csv")

all_clim_dat <- merge(SRGtowerdat, SRGffpdat,
                      by = c("yyyy", "mm", "day", "HH", "MM")) %>%
  transmute(
    TIMESTAMP_END,
    yyyy, mm, day, HH_UTC = HH, MM,
    zm = as.numeric(zm),
    umean = as.numeric(WS_1_1_1),
    h = as.numeric(1000),
    ol = as.numeric(L),
    sigmav = as.numeric(sigma_v),
    ustar = as.numeric(u_star),
    wind_dir = as.numeric(wind_dir),
    test = zm / ol,
    z0 = NaN
  ) %>%
  filter(test >= -15.5, ustar > 0.2)

# =====================================================================
# 3. Site coordinates
# =====================================================================
site_lat <- 31.7894
site_lon <- -110.8277

# =====================================================================
# 4. Cover fraction function
# =====================================================================
fast_cover_fraction <- function(ffp, classified_raster, site_lat, site_lon) {
  if (is.null(ffp$xr) || length(ffp$xr) == 0) {
    return(tibble(tree_prop = NA, grass_prop = NA, bare_prop = NA))
  }
  
  cover_vals <- map_dfr(seq_along(ffp$xr), function(i) {
    x <- ffp$xr[[i]]; y <- ffp$yr[[i]]
    idx <- complete.cases(x, y)
    if(sum(idx) == 0) return(NULL)
    
    mat <- cbind(x[idx], y[idx])
    lat <- (mat[,2] / 111111) + site_lat
    lon <- (mat[,1] / (111111 * cos(site_lat*pi/180))) + site_lon
    
    poly <- vect(cbind(lon, lat), type = "polygons", crs = "EPSG:4326")
    poly <- project(poly, crs(classified_raster))
    
    masked <- mask(classified_raster, poly)
    vals <- values(masked)
    valid <- !is.na(vals)
    if(sum(valid) == 0) return(NULL)
    
    tibble(
      tree = mean(vals[valid] == 1),
      grass = mean(vals[valid] == 2),
      bare = mean(vals[valid] == 3)
    )
  })
  
  tibble(
    tree_prop = if(nrow(cover_vals) > 0) mean(cover_vals$tree) * 100 else NA,
    grass_prop = if(nrow(cover_vals) > 0) mean(cover_vals$grass) * 100 else NA,
    bare_prop = if(nrow(cover_vals) > 0) mean(cover_vals$bare) * 100 else NA
  )
}

# =====================================================================
# 5. Single footprint wrapper for parallel execution
# =====================================================================
calc_single_FFP_wrapper <- function(row, site_lat, site_lon) {
  # Load raster inside worker
  classified_raster <- rast("./Data/RussHomework/testrast_reprojected.tif")
  
  tryCatch({
    ffp <- calc_footprint_FFP_climatology(
      zm = row$zm,
      z0 = row$z0,
      umean = row$umean,
      h = row$h,
      ol = row$ol,
      sigmav = row$sigmav,
      ustar = row$ustar,
      wind_dir = row$wind_dir,
      domain = c(-200, 200, -200, 200),
      r = seq(0, 90, by = 30)
    )
    
    cover <- fast_cover_fraction(ffp, classified_raster, site_lat, site_lon)
    
    tibble(
      TIMESTAMP_END = row$TIMESTAMP_END,
      yyyy = row$yyyy,
      mm = row$mm,
      day = row$day,
      HH = row$HH_UTC,
      MM = row$MM,
      wind_dir = row$wind_dir,
      umean = row$umean,
      ustar = row$ustar,
      zm = row$zm,
      L = row$ol,
      sigmav = row$sigmav,
      h = row$h,
      tree_prop = cover$tree_prop,
      grass_prop = cover$grass_prop,
      bare_prop = cover$bare_prop
    )
  }, error = function(e) {
    message("Skipping timestamp ", row$TIMESTAMP_END, ": ", e$message)
    tibble(
      TIMESTAMP_END = row$TIMESTAMP_END,
      yyyy = row$yyyy,
      mm = row$mm,
      day = row$day,
      HH = row$HH_UTC,
      MM = row$MM,
      wind_dir = row$wind_dir,
      umean = row$umean,
      ustar = row$ustar,
      zm = row$zm,
      L = row$ol,
      sigmav = row$sigmav,
      h = row$h,
      tree_prop = NA, grass_prop = NA, bare_prop = NA
    )
  })
}

# =====================================================================
# 6. Sequential footprint calculation (no parallel processing)
# =====================================================================

# Load raster once outside the loop to avoid reopening it each time
classified_raster <- rast("./Data/RussHomework/testrast_reprojected.tif")

# Optional: progress bar setup
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

# Calculate footprints sequentially
results <- list()

with_progress({
  p <- progressor(steps = nrow(all_clim_dat))
  
  for (i in seq_len(nrow(all_clim_dat))) {
    p(message = paste0("Processing ", i, "/", nrow(all_clim_dat)))
    
    res <- calc_single_FFP_wrapper(
      all_clim_dat[i, , drop = FALSE],
      site_lat,
      site_lon
    )
    
    results[[i]] <- res
  }
})

# Combine into a single dataframe
results <- bind_rows(results)
# =====================================================================
# 7. Save results
# =====================================================================
write.csv(results, "./Data/RussHomework/SRG_2024_halfhour_footprint_cover_Aug.csv", row.names = FALSE)

head(results)
