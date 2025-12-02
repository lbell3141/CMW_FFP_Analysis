library(terra)
library(dplyr)
library(lubridate)
library(tidyr)

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
  filter(WS_1_1_1 >= 0) %>%
  filter(HH %in% 8:17) %>%   # daytime only
  filter(mm %in% 5:9)

# =====================================================================
# 2. Load FFP input meteorology + merge with tower data
# =====================================================================
SRGffpdat <- read.csv("./Data/RussHomework/Input2dFootprint_SRG2024.csv")

timemerge <- merge(
  SRGtowerdat, SRGffpdat,
  by = c("yyyy", "mm", "day", "HH", "MM")
)

avg_WS <- mean(timemerge$WS_1_1_1, na.rm = TRUE)

# =====================================================================
# 3. Reformat for KLJUN footprint calculation
# =====================================================================
d <- 0.335
bound_h <- 1000

all_clim_data <- timemerge %>%
  transmute(
    TIMESTAMP_END,
    yyyy, mm, day, HH_UTC = HH, MM,
    zm = zm,
    umean = WS_1_1_1,        # <-- use actual half-hour WS, not avg
    h = bound_h,
    ol = L,
    sigmav = sigma_v,
    ustar = u_star,
    wind_dir = wind_dir,
    test = zm / L
  )

all_clim_dat <- all_clim_data %>%
  filter(test >= -15.5, ustar > 0.2) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  filter(HH_UTC %in% 8:17, mm %in% 5:9) %>%
  mutate(z0 = NaN)

# =====================================================================
# 4. Load classified raster (1=tree, 2=grass, 3=bare)
# =====================================================================
classified <- rast("./Data/RussHomework/testrast.tif")
site_lat <- 31.7894
site_lon <- -110.8277

# =====================================================================
# 5. Function to compute a half-hour footprint & cover fractions
# =====================================================================
calc_single_FFP <- function(row, classified, site_lat, site_lon) {
  
  # 1️⃣ Calculate FFP (contour-based)
  ffp <- calc_footprint_FFP_climatology(
    zm = row$zm,
    z0 = row$z0,       # can be NaN
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
  
  # 2️⃣ Handle empty footprint
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
  
  # 3️⃣ Loop over contour rings
  tree_vals <- c()
  grass_vals <- c()
  bare_vals <- c()
  
  for(i in seq_along(ffp$xr)) {
    
    x <- ffp$xr[[i]]
    y <- ffp$yr[[i]]
    idx <- complete.cases(x, y)
    if(sum(idx) == 0) next
    
    mat <- cbind(x[idx], y[idx])
    
    # Convert to lat/lon
    lat <- (mat[,2] / 111111) + site_lat
    lon <- (mat[,1] / (111111 * cos(site_lat*pi/180))) + site_lon
    
    # Polygon and projection
    poly <- vect(cbind(lon, lat), type="polygons", crs="EPSG:4326")
    poly <- project(poly, crs(classified))
    
    # Mask classified raster
    masked <- mask(classified, poly)
    vals <- values(masked)
    valid <- !is.na(vals)
    if(sum(valid) == 0) next
    
    # Compute mean per category (ring-level weighting)
    tree_vals  <- c(tree_vals, mean(vals[valid] == 1))
    grass_vals <- c(grass_vals, mean(vals[valid] == 2))
    bare_vals  <- c(bare_vals, mean(vals[valid] == 3))
  }
  
  # 4️⃣ Weighted cover: average across rings
  n <- length(tree_vals)
  tree  <- if(n > 0) mean(tree_vals) * 100 else NA
  grass <- if(n > 0) mean(grass_vals) * 100 else NA
  bare  <- if(n > 0) mean(bare_vals) * 100 else NA
  
  # 5️⃣ Return tibble
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
    tree_prop = tree,
    grass_prop = grass,
    bare_prop = bare
  )
}

# =====================================================================
# 6. Loop over all half-hour rows
# =====================================================================
all_clim_dat <- all_clim_dat %>%
  mutate(
    sigmav = as.numeric(sigmav),
    umean  = as.numeric(umean),
    ustar  = as.numeric(ustar),
    zm     = as.numeric(zm),
    h      = as.numeric(h),
    ol     = as.numeric(ol),
    wind_dir = as.numeric(wind_dir),
    z0 = as.numeric(z0)
  )


test <- all_clim_dat[1,]

results <- bind_rows(lapply(1:nrow(test), function(i) {
  calc_single_FFP(test[i,], classified, site_lat, site_lon)
}))



results <- bind_rows(
  lapply(1:nrow(test), function(i) {
    calc_single_FFP(test[i,], classified, site_lat, site_lon)
  })
)

write.csv(results, "./Data/RussHomework/SRG_2024_halfhour_footprint_cover.csv", row.names=FALSE)

head(results)
