#==================================================
# Plot Climatologies on NAIP Imagery with Footprints
#==================================================

library(terra)
library(tidyverse)
library(gridGraphics)
library(gridExtra)

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
  CMW = "./SeriousStuff/Data/NAIP_imagery/RawImagery/Premonsoon/NAIP_CMW_20170610.tif",
  SRM = "./SeriousStuff/Data/NAIP_imagery/RawImagery/Premonsoon/NAIP_SRM_20170611.tif",
  SRG = "./SeriousStuff/Data/NAIP_imagery/RawImagery/Premonsoon/NAIP_SRG_20170611.tif",
  WKG = "./SeriousStuff/Data/NAIP_imagery/RawImagery/Premonsoon/NAIP_Wkg_20170610.tif"
)

imagery <- lapply(imagery_files, rast)

#-------------------------------
# 2. Load footprint (ffp) objects
#-------------------------------
ffp_list <- list(
  CMW = readRDS("./SeriousStuff/Data/Footprints/SiteClimatologies/CMW_daytimeannual_2017ffp.rds"),
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

# Square crop around outer footprint
get_square_crop <- function(ffp_v, buffer = 100) {
  e <- ext(ffp_v$outer)
  max_range <- max(e[2]-e[1], e[4]-e[3])
  center_x <- (e[1] + e[2])/2
  center_y <- (e[3] + e[4])/2
  ext(center_x - max_range/2 - buffer,
      center_x + max_range/2 + buffer,
      center_y - max_range/2 - buffer,
      center_y + max_range/2 + buffer)
}

#-------------------------------
# 4. Create footprint polygons for each site
#-------------------------------
contour_idx <- c(5, 7, 9) # inner, mid, outer

site_ffp_vectors <- map(sites, function(site) {
  rast_crs <- crs(imagery[[site]])
  coords <- site_coords[[site]]
  ffp <- ffp_list[[site]]
  
  vects <- map(contour_idx, ~ ffp_to_vect(ffp, .x, coords["lat"], coords["lon"], rast_crs) |> smooth_poly(width = 15))
  names(vects) <- c("inner", "mid", "outer")
  vects
})
names(site_ffp_vectors) <- sites

#-------------------------------
# 5. Plot function: centered crosshair axes with bold ticks
#-------------------------------
plot_site_centered_axes_bold <- function(site, buffer = 50, tick_interval = 50) {
  r <- imagery[[site]]
  ffp_v <- site_ffp_vectors[[site]]
  
  # Crop to square around outer footprint
  fp_ext <- get_square_crop(ffp_v, buffer)
  r_crop <- crop(r, fp_ext)
  
  # Tower center in UTM
  center <- get_center_utm(site)
  e0 <- crds(center)[1, 1]
  n0 <- crds(center)[1, 2]
  
  # Plot RGB image
  par(mar = c(4,4,2,1))
  plotRGB(r_crop, r=1, g=2, b=3, stretch="hist", axes=FALSE, box=FALSE,
          xlim=c(fp_ext[1], fp_ext[2]), ylim=c(fp_ext[3], fp_ext[4]),
          asp=1, xaxs="i", yaxs="i")
  
  # Relative distances
  x_min <- fp_ext[1]-e0; x_max <- fp_ext[2]-e0
  y_min <- fp_ext[3]-n0; y_max <- fp_ext[4]-n0
  
  x_ticks <- seq(floor(x_min/tick_interval)*tick_interval,
                 ceiling(x_max/tick_interval)*tick_interval, by = tick_interval)
  y_ticks <- seq(floor(y_min/tick_interval)*tick_interval,
                 ceiling(y_max/tick_interval)*tick_interval, by = tick_interval)
  
  tick_len <- (fp_ext[2]-fp_ext[1])*0.02  # tick size
  
  # Draw crosshair axes
  segments(e0+x_min, n0, e0+x_max, n0, col="white", lwd=3)
  segments(e0, n0+y_min, e0, n0+y_max, col="white", lwd=3)
  
  # Horizontal ticks & labels
  segments(e0+x_ticks, n0-tick_len, e0+x_ticks, n0+tick_len, col="white", lwd=3)
  text(e0+x_ticks, n0-3*tick_len, labels=x_ticks, col="white", cex=1.5, font=2, adj=c(0.5,1))
  
  # Vertical ticks & labels
  segments(e0-tick_len, n0+y_ticks, e0+tick_len, n0+y_ticks, col="white", lwd=3)
  text(e0-3*tick_len, n0+y_ticks, labels=y_ticks, col="white", cex=1.5, font=2, adj=c(1,0.5))
  
  # Footprint polygons
  plot(ffp_v$outer, col=adjustcolor("yellow",0.15), border=NA, add=TRUE)
  plot(ffp_v$mid,   col=adjustcolor("orange",0.25), border=NA, add=TRUE)
  plot(ffp_v$inner, col=adjustcolor("red",0.35), border=NA, add=TRUE)
  lines(ffp_v$outer, col="yellow", lwd=2)
  
  # Site title
  #title(site, line=0.5, col.main="white", font.main=2)
}

#-------------------------------
# 6. Test plot
#-------------------------------
plot_site_centered_axes_bold("CMW")
plot_site_centered_axes_bold("SRM")
plot_site_centered_axes_bold("SRG")
plot_site_centered_axes_bold("WKG")
