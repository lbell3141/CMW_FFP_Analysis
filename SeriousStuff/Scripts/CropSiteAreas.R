#buffer 500m side length square around each site
# CRS WGS84 / UTM Zone 12N (EPSG:32612)

# CMW <- 	(31.6637, -110.1777)
# SRG <- (31.7894, -110.8277)
# SRM <- (31.8214, -110.8661)
# WKG <- 	(31.7365, -109.9419)

library(sf)
library(dplyr)
library(tibble)

make_square_xy <- function(x, y, side_m, crs) {
  
  half <- side_m / 2
  
  polys <- lapply(seq_along(x), function(i) {
    st_polygon(list(matrix(
      c(x[i] - half, y[i] - half,
        x[i] + half, y[i] - half,
        x[i] + half, y[i] + half,
        x[i] - half, y[i] + half,
        x[i] - half, y[i] - half),
      ncol = 2,
      byrow = TRUE
    )))
  })
  
  st_sfc(polys, crs = crs)
}

sites <- tibble(
  site = c("CMW", "SRG", "SRM", "WKG"),
  lat  = c(31.6637, 31.7894, 31.8214, 31.7365),
  lon  = c(-110.1777, -110.8277, -110.8661, -109.9419)
)

sites_sf <- st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 4326
) |>
  st_transform(32612)

coords <- st_coordinates(sites_sf)

squares_utm <- st_sf(
  site = sites_sf$site,
  geometry = make_square_xy(
    x = coords[, "X"],
    y = coords[, "Y"],
    side_m = 500,
    crs = st_crs(sites_sf)
  )
)


out_dir <- "./SeriousStuff/Data/CropSiteArea"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(squares_utm))) {
  st_write(
    squares_utm[i, ],
    paste0(out_dir, "/", squares_utm$site[i], "_500m_square.shp"),
    delete_layer = TRUE,
    quiet = TRUE
  )
}