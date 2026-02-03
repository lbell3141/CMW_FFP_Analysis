#create a raster stack for all sites with LAI, NDVI, canopy cover, height, TWI(, and soil?)
#mask raster stack with directional ffps and create df with direction and premonsoon data

library(terra)
library(tidyverse)
library(lubridate)
library(purrr)

sites <- c("CMW", "SRM", "SRG", "Wkg")

site_coords <- list(
  CMW = c(31.6637, -110.1777),
  SRG = c(31.7894, -110.8277),
  SRM = c(31.8214, -110.8661),
  Wkg = c(31.7365, -109.9419)
)

dirs <- c(
  cancov_dir = "./SeriousStuff/Data/LiDAR/CanopyCover",
  chm_dir    = "./SeriousStuff/Data/LiDAR/CHM",
  ndvi_dir   = "./SeriousStuff/Data/NAIP_imagery/NAIP-derived_NDVI/premonsoon/2017",
  lai_dir    = "./SeriousStuff/Data/NAIP_imagery/NAIP-derived_LAI/premonsoon/2017",
  twi_dir    = "./SeriousStuff/Data/USGS_3DEP_DEM/3DEP-derived_TWI"
)

sites <- c("CMW", "SRM", "SRG", "Wkg")

# find rast files from all folders
all_files <- unlist(
  lapply(dirs, function(d) {
    list.files(d, pattern = "\\.tif$", full.names = TRUE)
  })
)

#read in rasts for each site
build_site_rasters <- function(site, files) {

  site_files <- files[grepl(site, basename(files), fixed = TRUE)]

  if (length(site_files) == 0) {
    warning(paste("No rasters found for site:", site))
    return(NULL)
  }

  lapply(site_files, rast)
}

site_raster_lists <- setNames(
  lapply(sites, build_site_rasters, files = all_files),
  sites
)

# choose rast with highest res to serve as a template
pick_template <- function(r_list) {
  res_vals <- sapply(r_list, function(r) prod(res(r)))
  r_list[[which.min(res_vals)]]
}

# align raster grid cells ( resample coarser resolutions)
align_rasters <- function(r_list) {

  template <- pick_template(r_list)

  lapply(r_list, function(r) {

    # Fix CRS if needed
    if (!compareGeom(
      r, template,
      crs = TRUE, ext = FALSE,
      rowcol = FALSE, res = FALSE,
      stopOnError = FALSE
    )) {
      r <- project(r, template)
    }

    resample(r, template)
  })
}
# crop to extent of smallest raster
crop_to_common_extent <- function(r_list) {

  common_ext <- Reduce(intersect, lapply(r_list, ext))

  lapply(r_list, function(r) crop(r, common_ext))
}

#create raster stacks
stack_rasters <- function(r_list) rast(r_list)

# apply all the functions above to the geospat raster data
site_raster_lists_aligned <- lapply(
  site_raster_lists,
  align_rasters
)

site_raster_lists_cropped <- lapply(
  site_raster_lists_aligned,
  crop_to_common_extent
)

site_stacks <- lapply(
  site_raster_lists_cropped,
  stack_rasters
)

#saveRDS(site_stacks, "./SeriousStuff/Data/RasterStack/site_stacks.RDS")
plot(site_stacks$CMW)
#===============================================================================
#apply directional ffps to data

#site_stacks <- readRDS("./SeriousStuff/Data/RasterStack/site_stacks.RDS")

ffp_list <- list(
  CMW = readRDS("./SeriousStuff/Data/Footprints/DirectionalFFPs/CMW_MJJ24_ffp_list.rds"),
  SRM = readRDS("./SeriousStuff/Data/Footprints/DirectionalFFPs/SRM_MJJ24_ffp_list.rds"),
  SRG = readRDS("./SeriousStuff/Data/Footprints/DirectionalFFPs/SRG_MJJ24_ffp_list.rds"),
  Wkg = readRDS("./SeriousStuff/Data/Footprints/DirectionalFFPs/Wkg_MJJ24_ffp_list.rds")
)

#weighting rasts by contour function
ffp_contours_to_mask <- function(xr_list, yr_list, rast, site_center_lat, site_center_lon, weights = c(0.3,0.3,0.3)) {
  
  # Convert to numeric matrices
  coord_pair_list <- lapply(seq_along(xr_list), function(i) {
    df <- data.frame(x = as.numeric(xr_list[[i]]), y = as.numeric(yr_list[[i]]))
    df <- na.omit(df)
    as.matrix(df)
  })
  
  # Convert to lat/lon
  latlon_list <- lapply(coord_pair_list, function(mat) {
    lat <- (mat[,2]/111111) + site_center_lat
    lon <- (mat[,1]/(111111 * cos(site_center_lat * pi/180))) + site_center_lon
    cbind(lon, lat)
  })
  
  # SpatVector lines
  lines_list <- lapply(latlon_list, function(mat) terra::vect(mat, type="lines", crs="EPSG:4326"))
  
  # Reproject to raster CRS
  lines_list <- lapply(lines_list, function(line) terra::project(line, terra::crs(rast)))
  
  # Select contour lines (adjust indices if needed)
  part_con_list <- list(lines_list[[5]], lines_list[[7]], lines_list[[9]])
  
  # Mask raster
  masked_rast <- lapply(part_con_list, function(line) terra::mask(rast, line))
  
  # Subtract inner contours
  dif_con_list <- list(
    masked_rast[[1]],
    terra::mask(masked_rast[[2]], masked_rast[[1]], inverse=TRUE),
    terra::mask(masked_rast[[3]], masked_rast[[2]], inverse=TRUE)
  )
  
  # Compute mean values
  vals <- sapply(dif_con_list, function(r) mean(terra::values(r), na.rm=TRUE))
  
  # Weighted sum
  sum(vals * weights)
}

# use weighting function results to make df
compute_site_ffp <- function(site_name, stack, ffp_list_site, site_lat, site_lon) {
  
  x_list <- lapply(ffp_list_site, function(ffp) ffp$xr)
  y_list <- lapply(ffp_list_site, function(ffp) ffp$yr)
  
  map_dfr(seq_len(nlyr(stack)), function(i) {
    
    layer <- stack[[i]]
    
    veg_vals <- mapply(
      ffp_contours_to_mask,
      x_list,
      y_list,
      MoreArgs = list(
        rast = layer,
        site_center_lat = site_lat,
        site_center_lon = site_lon
      )
    )
    
    data.frame(
      site = site_name,
      layer = names(layer),
      direction = seq(20, 360, by=20)[1:length(veg_vals)],
      veg_cover = veg_vals
    )
  })
}

# Loop through sites and apply masking function and function to store results in df
results_list <- map(names(site_stacks), function(site) {
  compute_site_ffp(
    site_name = site,
    stack = site_stacks[[site]],
    ffp_list_site = ffp_list[[site]],
    site_lat = site_coords[[site]][1],
    site_lon = site_coords[[site]][2]
  )
})

# Combine into one df + save
ffp_results_df <- bind_rows(results_list)%>%
  rename(value = veg_cover)
#saveRDS(ffp_results_df, "./SeriousStuff/Data/RasterStack/DirGeosDf468.rds")

