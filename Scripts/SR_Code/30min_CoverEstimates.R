library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

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
  filter(WS_1_1_1 >= 0, HH %in% 8:17, mm %in% 7:9)

# =====================================================================
# 2. Load FFP input meteorology + merge with tower data
# =====================================================================
SRGffpdat <- read.csv("./Data/RussHomework/Input2dFootprint_SRG2024.csv")

timemerge <- merge(SRGtowerdat, SRGffpdat,
                   by = c("yyyy", "mm", "day", "HH", "MM"))

# =====================================================================
# 3. Reformat for KLJUN footprint calculation
# =====================================================================
all_clim_dat <- timemerge %>%
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
# 4. Load classified raster
# =====================================================================
classified <- rast("./Data/RussHomework/testrast.tif")
site_lat <- 31.7894
site_lon <- -110.8277

# =====================================================================
# 5. Function to compute a half-hour footprint & cover fractions
# =====================================================================
calc_single_FFP <- function(row, classified, site_lat, site_lon) {
  ffp <- calc_footprint_FFP_climatology(
    zm = row$zm,
    z0 = row$z0,
    umean = row$umean,
    h = row$h,
    ol = row$ol,
    sigmav = row$sigmav,
    ustar = row$ustar,
    wind_dir = row$wind_dir,
    fig = 0,
    domain = c(-200, 200, -200, 200),
    r = seq(0, 90, by = 10)
  )
  
  if (is.null(ffp$xr) || length(ffp$xr) == 0) {
    return(tibble(
      TIMESTAMP_END = row$TIMESTAMP_END,
      yyyy = row$yyyy, mm = row$mm, day = row$day,
      HH = row$HH_UTC, MM = row$MM,
      wind_dir = row$wind_dir,
      umean = row$umean, ustar = row$ustar,
      zm = row$zm, L = row$ol,
      sigmav = row$sigmav, h = row$h,
      tree_prop = NA, grass_prop = NA, bare_prop = NA
    ))
  }
  
  # Compute weighted cover fractions
  cover_vals <- map_dfr(seq_along(ffp$xr), function(i) {
    x <- ffp$xr[[i]]; y <- ffp$yr[[i]]
    idx <- complete.cases(x, y)
    if(sum(idx) == 0) return(NULL)
    
    mat <- cbind(x[idx], y[idx])
    lat <- (mat[,2] / 111111) + site_lat
    lon <- (mat[,1] / (111111 * cos(site_lat*pi/180))) + site_lon
    poly <- vect(cbind(lon, lat), type="polygons", crs="EPSG:4326")
    poly <- project(poly, crs(classified))
    masked <- mask(classified, poly)
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
    TIMESTAMP_END = row$TIMESTAMP_END,
    yyyy = row$yyyy, mm = row$mm, day = row$day,
    HH = row$HH_UTC, MM = row$MM,
    wind_dir = row$wind_dir,
    umean = row$umean, ustar = row$ustar,
    zm = row$zm, L = row$ol,
    sigmav = row$sigmav, h = row$h,
    tree_prop = if(nrow(cover_vals) > 0) mean(cover_vals$tree) * 100 else NA,
    grass_prop = if(nrow(cover_vals) > 0) mean(cover_vals$grass) * 100 else NA,
    bare_prop = if(nrow(cover_vals) > 0) mean(cover_vals$bare) * 100 else NA
  )
}

# =====================================================================
# 6. Compute footprints in parallel using purrr
# =====================================================================
library(purrr)
results <- map_dfr(seq_len(nrow(all_clim_dat)), function(i) {
  row <- all_clim_dat[i, , drop = FALSE]
  
  # wrap in tryCatch to skip problematic rows
  tryCatch({
    calc_single_FFP(row, classified, site_lat, site_lon)
  }, error = function(e) {
    message("Skipping row ", i, ": ", e$message)
    tibble(
      TIMESTAMP_END = row$TIMESTAMP_END,
      yyyy = row$yyyy, mm = row$mm, day = row$day,
      HH = row$HH_UTC, MM = row$MM,
      wind_dir = row$wind_dir,
      umean = row$umean, ustar = row$ustar,
      zm = row$zm, L = row$ol,
      sigmav = row$sigmav, h = row$h,
      tree_prop = NA, grass_prop = NA, bare_prop = NA
    )
  })
})

write.csv(results, "./Data/RussHomework/SRG_2024_halfhour_footprint_cover.csv", row.names = FALSE)

head(results)
