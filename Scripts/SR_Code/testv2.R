library(furrr)
library(terra)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

source("./Scripts/kljun_code/calc_footprint_FFP_climatology.R")
terra_extract <- terra::extract

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
# 3. Load raster and prepare for fast extraction
# =====================================================================
classified <- rast("./Data/RussHomework/testrast.tif")
target_crs <- "EPSG:32612"
classified_proj <- project(classified, target_crs)

# Pre-extract raster values and store geometry info
classified_vals <- terra::values(classified_proj)
ncol_classified <- ncol(classified_proj)

# Tower coordinates in projected CRS
tower_pt <- vect(cbind(-110.8277, 31.7894), crs="EPSG:4326", type="points")
tower_pt_proj <- project(tower_pt, target_crs)
tower_x <- geom(tower_pt_proj)[1,1]
tower_y <- geom(tower_pt_proj)[1,2]

# =====================================================================
# 4. Clean footprint function
# =====================================================================
calc_single_FFP_fast <- function(row, tower_x, tower_y, classified_proj) {
  # 1. Compute footprint
  ffp <- tryCatch({
    calc_footprint_FFP_climatology(
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
  }, error = function(e) NULL)
  
  if (is.null(ffp) || length(ffp$xr) == 0) {
    return(tibble(
      TIMESTAMP_END = row$TIMESTAMP_END,
      yyyy = row$yyyy, mm = row$mm, day = row$day,
      HH = row$HH_UTC, MM = row$MM,
      wind_dir = row$wind_dir, umean = row$umean, ustar = row$ustar,
      zm = row$zm, L = row$ol, sigmav = row$sigmav, h = row$h,
      tree_prop = NA, grass_prop = NA, bare_prop = NA
    ))
  }
  
  # Flatten footprint coordinates
  coords <- map2(ffp$xr, ffp$yr, function(x, y) {
    if (is.null(x) || is.null(y)) return(NULL)
    if (length(x) == 0 || length(y) == 0) return(NULL)
    data.frame(x = x, y = y)
  }) %>% purrr::compact() %>% bind_rows() %>% filter(complete.cases(x, y))
  
  if (nrow(coords) == 0) {
    return(tibble(
      TIMESTAMP_END = row$TIMESTAMP_END,
      yyyy = row$yyyy, mm = row$mm, day = row$day,
      HH = row$HH_UTC, MM = row$MM,
      wind_dir = row$wind_dir, umean = row$umean, ustar = row$ustar,
      zm = row$zm, L = row$ol, sigmav = row$sigmav, h = row$h,
      tree_prop = NA, grass_prop = NA, bare_prop = NA
    ))
  }
  
  # Convert offsets to projected coordinates
  px <- coords$x + tower_x
  py <- coords$y + tower_y
  
  pts <- vect(cbind(px, py), type = "points", crs = crs(classified_proj))
  
  ex <- terra::extract(classified_proj, pts, ID = FALSE)
  vals <- ex[[1]]
  valid <- !is.na(vals)
  
  if (!any(valid)) {
    tree_prop <- grass_prop <- bare_prop <- NA
  } else {
    tree_prop  <- mean(vals[valid] == 1) * 100
    grass_prop <- mean(vals[valid] == 2) * 100
    bare_prop  <- mean(vals[valid] == 3) * 100
  }
  
  tibble(
    TIMESTAMP_END = row$TIMESTAMP_END,
    yyyy = row$yyyy, mm = row$mm, day = row$day,
    HH = row$HH_UTC, MM = row$MM,
    wind_dir = row$wind_dir,
    umean = row$umean, ustar = row$ustar,
    zm = row$zm, L = row$ol,
    sigmav = row$sigmav, h = row$h,
    tree_prop = tree_prop,
    grass_prop = grass_prop,
    bare_prop = bare_prop
  )
}

# =====================================================================
# 5. Parallel processing
# =====================================================================
plan(multisession, workers = parallel::detectCores() - 1)
all_clim_dat <- all_clim_dat[1,]
results <- map_dfr(seq_len(nrow(all_clim_dat)), function(i) {
  calc_single_FFP_fast(all_clim_dat[i, ], tower_x = tower_x, tower_y = tower_y, classified_proj = classified_proj)
})

# Save results
write.csv(results, "./Data/RussHomework/SRG_2024_halfhour_footprint_cover_fast.csv", row.names = FALSE)
