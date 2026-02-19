#==================================================
# Plot Climatologies on NAIP Imagery with Footprints
#==================================================

library(terra)
library(tidyverse)

#-------------------------------
# 1. Site coordinates & imagery
#-------------------------------
sites <- c("CMW", "SRM", "SRG", "WKG")

site_coords <- list(
  CMW = c(lat = 31.6637, lon = -110.1777),
  SRM = c(lat = 31.8214, lon = -110.8661),
  SRG = c(lat = 31.7894, lon = -110.8277),
  WKG = c(lat = 31.7365, lon = -109.9419)
)

imagery_files <- list(
  CMW = "./SeriousStuff/Data/NAIP_imagery/1000m/RawImagery/postmonsoon/NAIP_CMW_20231016.tif",
  SRM = "./SeriousStuff/Data/NAIP_imagery/1000m/RawImagery/postmonsoon/NAIP_SRM_20231018.tif",
  SRG = "./SeriousStuff/Data/NAIP_imagery/1000m/RawImagery/postmonsoon/NAIP_SRG_20231018.tif",
  WKG = "./SeriousStuff/Data/NAIP_imagery/1000m/RawImagery/postmonsoon/NAIP_Wkg_20231016.tif"
)

imagery <- lapply(imagery_files, rast)

#-------------------------------
# 2. Load footprint (ffp) objects
#-------------------------------
ffp_list <- list(
  CMW = readRDS("./SeriousStuff/Data/Footprints/SiteClimatologies/CMW_daytimeannual_ffp2021.rds"),
  SRM = readRDS("./SeriousStuff/Data/Footprints/SiteClimatologies/SRM_daytimeannual_ffp.rds"),
  SRG = readRDS("./SeriousStuff/Data/Footprints/SiteClimatologies/SRG_daytimeannual_ffp.rds"),
  WKG = readRDS("./SeriousStuff/Data/Footprints/SiteClimatologies/Wkg_daytimeannual_ffp.rds")
)

#-------------------------------
# 3. Helper functions
#-------------------------------

# Convert ffp contour to vector polygon in raster CRS
ffp_to_vect <- function(ffp, idx, site_lat, site_lon, rast_crs) {
  xr <- as.numeric(ffp$xr[[idx]])
  yr <- as.numeric(ffp$yr[[idx]])
  df <- na.omit(data.frame(x = xr, y = yr))
  df <- rbind(df, df[1, ]) # close polygon
  lon <- (df$x / (111111 * cos(site_lat * pi / 180))) + site_lon
  lat <- (df$y / 111111) + site_lat
  v <- vect(cbind(lon, lat), type = "polygons", crs = "EPSG:4326")
  project(v, rast_crs)
}

# Smooth polygon (buffer in/out)
smooth_poly <- function(v, width = 10) {
  v |> buffer(width = width) |> buffer(width = -width)
}

# Tower center in raster CRS
get_center_utm <- function(site) {
  coords <- site_coords[[site]]
  p <- vect(matrix(c(coords["lon"], coords["lat"]), ncol = 2), crs = "EPSG:4326")
  project(p, crs(imagery[[site]]))
}

#-------------------------------
# 4. Create footprint polygons for each site
#-------------------------------
contour_idx <- c(5, 7, 9) # inner, mid, outer

site_ffp_vectors <- map(sites, function(site) {
  rast_crs <- crs(imagery[[site]])
  coords <- site_coords[[site]]
  ffp <- ffp_list[[site]]
  
  vects <- map(
    contour_idx,
    ~ ffp_to_vect(ffp, .x, coords["lat"], coords["lon"], rast_crs) |>
      smooth_poly(width = 15)
  )
  
  names(vects) <- c("inner", "mid", "outer")
  vects
})

names(site_ffp_vectors) <- sites

#-------------------------------
# 5. Plot function: centered crosshair axes with bold ticks
#-------------------------------

plot_site_centered_axes_bold <- function(site,
                                         half_width = 350,
                                         tick_interval = 50) {
  
  r <- imagery[[site]]
  ffp_v <- site_ffp_vectors[[site]]
  
  # Tower center
  center <- get_center_utm(site)
  e0 <- crds(center)[1, 1]
  n0 <- crds(center)[1, 2]
  
  # Fixed 350 m extent
  fp_ext <- ext(
    e0 - half_width,
    e0 + half_width,
    n0 - half_width,
    n0 + half_width
  )
  
  r_crop <- crop(r, fp_ext)
  
  # RGB contrast stretch (5–95%)
  rgb_limits <- lapply(1:3, function(i) {
    q <- quantile(values(r_crop[[i]]),
                  probs = c(0.05, 0.95),
                  na.rm = TRUE)
    as.numeric(q)
  })
  
  par(mar = c(4,4,2,1))
  
  plotRGB(
    r_crop,
    r = 1, g = 2, b = 3,
    stretch = "lin",
    colNA = "black",
    axes = FALSE,
    box = FALSE,
    asp = 1,
    xaxs = "i",
    yaxs = "i",
    scale = max(unlist(rgb_limits))
  )
  
  # Relative ranges
  x_ticks <- seq(-half_width, half_width, by = tick_interval)
  y_ticks <- seq(-half_width, half_width, by = tick_interval)
  
  tick_len <- (2 * half_width) * 0.03  # slightly longer ticks
  
  # ---------------------------
  # Bold crosshair axes
  # ---------------------------
  segments(e0 - half_width, n0,
           e0 + half_width, n0,
           col = "white", lwd = 4)
  
  segments(e0, n0 - half_width,
           e0, n0 + half_width,
           col = "white", lwd = 4)
  
  # ---------------------------
  # Extra-bold tick marks
  # ---------------------------
  segments(e0 + x_ticks, n0 - tick_len,
           e0 + x_ticks, n0 + tick_len,
           col = "white", lwd = 5)
  
  segments(e0 - tick_len, n0 + y_ticks,
           e0 + tick_len, n0 + y_ticks,
           col = "white", lwd = 5)
  
  # ---------------------------
  # Footprints
  # ---------------------------
  plot(ffp_v$outer,
       col = adjustcolor("yellow", 0.35),
       border = NA,
       add = TRUE)
  
  plot(ffp_v$mid,
       col = adjustcolor("orange", 0.45),
       border = NA,
       add = TRUE)
  
  plot(ffp_v$inner,
       col = adjustcolor("red", 0.35),
       border = NA,
       add = TRUE)
  
  lines(ffp_v$outer, col = "yellow", lwd = 3)
}

#-------------------------------
# 6. Test plot
#-------------------------------
plot_site_centered_axes_bold("CMW")
plot_site_centered_axes_bold("SRM")
plot_site_centered_axes_bold("SRG")
plot_site_centered_axes_bold("WKG")


# walk(sites, ~{
#   png(
#     filename = paste0(.x, "_FFP_climatology_350m.png"),
#     width = 2000,
#     height = 2000,
#     res = 300,
#     bg = "black"
#   )
#   plot_site_centered_axes_bold(.x)
#   dev.off()
# })

