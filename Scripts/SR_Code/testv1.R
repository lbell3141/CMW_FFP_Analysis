library(furrr)
library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

# Load KLJUN footprint function
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

all_clim_dat <- merge(SRGtowerdat, SRGffpdat,
                      by = c("yyyy", "mm", "day", "HH", "MM")) %>%
  transmute(
    TIMESTAMP_END,
    yyyy, mm, day, HH_UTC = HH, MM,
    zm = as.numeric(zm),
    umean = as.numeric(WS_1_1_1),
    h = 1000,
    ol = as.numeric(L),
    sigmav = as.numeric(sigma_v),
    ustar = as.numeric(u_star),
    wind_dir = as.numeric(wind_dir),
    test = zm / ol,
    z0 = NaN
  ) %>%
  filter(test >= -15.5, ustar > 0.2)

# =====================================================================
# 3. Load and project classified raster
# =====================================================================
classified <- rast("./Data/RussHomework/testrast.tif")
target_crs <- "EPSG:32612"
classified_proj <- project(classified, target_crs)

# Tower location in projected coordinates
tower_pt <- vect(cbind(-110.8277, 31.7894), crs="EPSG:4326", type="points")
tower_pt_proj <- project(tower_pt, target_crs)
tower_x <- geom(tower_pt_proj)[1,1]
tower_y <- geom(tower_pt_proj)[1,2]

# =====================================================================
# 4. Footprint extraction function
# =====================================================================
calc_single_FFP_fast <- function(row, classified_proj, tower_x, tower_y) {
  message("Calculating footprint for ", row$TIMESTAMP_END)
  
  ffp <- tryCatch({
    calc_footprint_FFP_climatology(
      zm = row$zm, z0 = row$z0, umean = row$umean, h = row$h,
      ol = row$ol, sigmav = row$sigmav, ustar = row$ustar,
      wind_dir = row$wind_dir, fig = 0,
      domain = c(-200, 200, -200, 200), r = seq(0, 90, by = 10)
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
  coords <- map2(ffp$xr, ffp$yr, ~ if(length(.x) > 0 & length(.y) > 0) data.frame(x=.x, y=.y) else NULL) %>%
    purrr::compact() %>%
    bind_rows()
  
  if (nrow(coords) == 0) return(NULL)
  
  # Convert offsets to projected coordinates
  px <- coords$x + tower_x
  py <- coords$y + tower_y
  pts <- vect(cbind(px, py), type="points", crs=crs(classified_proj))
  
  # Extract raster cell values
  vals_classified <- terra::extract(classified_proj, pts)[[1]]
  valid <- !is.na(vals_classified)
  
  # Compute proportions
  tree_prop  <- if(any(valid)) mean(vals_classified[valid] == 1) * 100 else NA
  grass_prop <- if(any(valid)) mean(vals_classified[valid] == 2) * 100 else NA
  bare_prop  <- if(any(valid)) mean(vals_classified[valid] == 3) * 100 else NA
  
  tibble(
    TIMESTAMP_END = row$TIMESTAMP_END,
    yyyy = row$yyyy, mm = row$mm, day = row$day,
    HH = row$HH_UTC, MM = row$MM,
    wind_dir = row$wind_dir, umean = row$umean, ustar = row$ustar,
    zm = row$zm, L = row$ol, sigmav = row$sigmav, h = row$h,
    tree_prop = tree_prop, grass_prop = grass_prop, bare_prop = bare_prop
  )
}

# =====================================================================
# 5. Run footprints in parallel
# =====================================================================
library(future)
plan(multisession, workers = parallel::detectCores() - 1)
all_clim_dat <- all_clim_dat[1,]
results <- map_dfr(
  seq_len(nrow(all_clim_dat)),
  ~ calc_single_FFP_fast(
    all_clim_dat[.x, ],
    classified_proj = classified_proj,
    tower_x = tower_x,
    tower_y = tower_y
  )
)

# =====================================================================
# 6. Save results
# =====================================================================
write.csv(results, "./Data/RussHomework/SRG_2024_halfhour_footprint_cover_fast.csv", row.names = FALSE)
