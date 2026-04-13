
library(terra)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(lubridate)
library(progress)

############################################################
# ALL INPUT DATA

sites <- c("CMW", "SRM", "SRG", "WKG")

site_coords <- list(
  CMW = c(31.6637, -110.1777),
  SRG = c(31.7894, -110.8277),
  SRM = c(31.8214, -110.8661),
  WKG = c(31.7365, -109.9419)
)

ffp_list <- list(
  CMW = readRDS("./SeriousStuff/Data/Footprints/DirectionalFFPs/CMW_2021_ffp_list.rds"),
  SRM = readRDS("./SeriousStuff/Data/Footprints/DirectionalFFPs/SRM_ffp_list.rds"),
  SRG = readRDS("./SeriousStuff/Data/Footprints/DirectionalFFPs/SRG_ffp_list.rds"),
  WKG = readRDS("./SeriousStuff/Data/Footprints/DirectionalFFPs/WKG_ffp_list.rds")
)

dirs <- c(
  cancov_dir = "./SeriousStuff/Data/LiDAR/CanopyCover",
  chm_dir    = "./SeriousStuff/Data/LiDAR/CHM",
  ndvi_dir   = "./SeriousStuff/Data/NAIP_imagery/1000m/NAIP-derived_NDVI/all",
  lai_dir    = "./SeriousStuff/Data/NAIP_imagery/1000m/NAIP-derived_LAI/all",
  twi_dir    = "./SeriousStuff/Data/USGS_3DEP_DEM/3DEP-derived_TWI"
)

all_files <- unlist(
  lapply(dirs, function(d) {
    list.files(d, pattern = "\\.tif$", full.names = TRUE)
  })
)

extract_date_from_name <- function(filename) {
  date_str <- str_extract(basename(filename), "\\d{8}")
  as.Date(date_str, format = "%Y%m%d")
}

raster_metadata <- map_dfr(all_files, function(f) {
  
  date_val <- extract_date_from_name(f)
  
  tibble(
    file = f,
    site = str_extract(basename(f), "CMW|SRM|SRG|WKG"),
    variable = case_when(
      str_detect(f, regex("CHM", ignore_case = TRUE)) ~ "chm",
      str_detect(f, regex("Canopy", ignore_case = TRUE)) ~ "cancov",
      str_detect(f, regex("NDVI", ignore_case = TRUE)) ~ "ndvi",
      str_detect(f, regex("LAI", ignore_case = TRUE)) ~ "lai",
      str_detect(f, regex("TWI", ignore_case = TRUE)) ~ "twi",
      TRUE ~ NA_character_
    ),
    yyyy = year(date_val),
    mm   = month(date_val)
  )
})

site_files <- list(
  CMW = "./SeriousStuff/Data/RandomForestOutputs/cmwmm_rf_summary_resultsPI.RDS",
  SRG = "./SeriousStuff/Data/RandomForestOutputs/srgmm_rf_summary_resultsPI.RDS",
  SRM = "./SeriousStuff/Data/RandomForestOutputs/srmmm_rf_summary_resultsPI.RDS",
  WKG = "./SeriousStuff/Data/RandomForestOutputs/wkgmm_rf_summary_resultsPI.RDS"
)

site_summaries <- imap(site_files, ~{
  readRDS(.x) %>%
    bind_rows(.id = "ym_id") %>%
    mutate(site = .y)
})

all_sites_df <- bind_rows(site_summaries)

split_ym_df <- all_sites_df %>%
  rename(ym = ym_id) %>%
  separate(ym, into = c("yyyy", "mm"), sep = "\\.", convert = TRUE) %>%
  select(site, yyyy, mm, direction, diff_avg_gpp) %>%
  rename(residual = diff_avg_gpp)

direction_bins <- seq(20, 360, 20)

############################################################
# WRITE GEOMETRY

precomputed_ffp <- list()

for (site in sites) {
  
  ffp_site <- ffp_list[[site]]
  
  precomputed_ffp[[site]] <- map(seq_along(ffp_site), function(i) {
    list(
      xr = ffp_site[[i]]$xr,
      yr = ffp_site[[i]]$yr,
      direction = direction_bins[i]
    )
  })
}

############################################################
# PROCESS EACH RASTER

n_rasters <- nrow(raster_metadata)

library(progress)

pb <- progress_bar$new(
  format = "Raster [:bar] :percent | :current/:total | ETA: :eta",
  total = n_rasters,
  clear = FALSE,
  width = 70
)

raster_results <- vector("list", n_rasters)

for (i in seq_len(n_rasters)) {
  
  row <- raster_metadata[i,]
  
  site      <- row$site
  variable  <- row$variable
  yyyy      <- row$yyyy
  mm        <- row$mm
  file      <- row$file
  
  cat("\nProcessing raster:", basename(file), "\n")
  
  r <- rast(file)
  
  site_center <- site_coords[[site]]
  masks       <- precomputed_ffp[[site]]
  
  dir_vals <- map_dfr(masks, function(mask_info) {
    
    val <- ffp_contours_to_mask(
      mask_info$xr,
      mask_info$yr,
      rast = r,
      site_center_lat = site_center[1],
      site_center_lon = site_center[2]
    )
    
    tibble(
      direction = mask_info$direction,
      weighted_mean = val
    )
  })
  
  raster_results[[i]] <- dir_vals %>%
    mutate(
      site = site,
      yyyy = yyyy,
      mm   = mm,
      variable = variable
    )
  
  rm(r); gc()
  pb$tick()
}




spatial_df <- bind_rows(raster_results)

#write.csv(spatial_df, "./SeriousStuff/Data/RasterStack/MaskedSpatialData.csv")
