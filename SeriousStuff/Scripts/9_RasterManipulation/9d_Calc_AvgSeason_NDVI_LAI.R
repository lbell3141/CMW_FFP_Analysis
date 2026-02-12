# Calc the average values for NDVI and LAI rasters for each monsoon season at each site

library(terra)
library(tidyverse)
library(lubridate)

# Directories
ndvi_dir <- "./SeriousStuff/Data/NAIP_imagery/1000m/NAIP-derived_NDVI"
lai_dir  <- "./SeriousStuff/Data/NAIP_imagery/1000m/NAIP-derived_LAI"

out_ndvi_dir <- "./SeriousStuff/Data/NAIP_imagery/1000m/SeasonalMean_NDVI"
out_lai_dir  <- "./SeriousStuff/Data/NAIP_imagery/1000m/SeasonalMean_LAI"

seasons <- c("premonsoon", "monsoon", "postmonsoon", "winter")

# dir.create(out_ndvi_dir, recursive = TRUE, showWarnings = FALSE)
# dir.create(out_lai_dir,  recursive = TRUE, showWarnings = FALSE)

# Helper func for NDVI/LAI file names
parse_naip_name <- function(fname) {
  
  site <- case_when(
    str_detect(fname, "^LAI_")  ~ str_extract(fname, "(?<=LAI_)[A-Z]{3}"),
    TRUE                        ~ str_extract(fname, "(?<=^)[A-Z]{3}(?=_)")
  )
  
  date <- str_extract(fname, "\\d{8}")
  
  tibble(
    site = toupper(site),
    date = ymd(date)
  )
}


# Core averaging function
# note- extents are slightly off, so takes a template raster for each site from the list
# calcs standard mean
seasonal_site_mean <- function(in_dir, out_dir, pattern, varname) {
  
  for (season in seasons) {
    
    message(varname, " mean → ", season)
    
    season_dir <- file.path(in_dir, season)
    if (!dir.exists(season_dir)) next
    
    files <- list.files(season_dir, pattern = pattern, full.names = TRUE)
    if (length(files) == 0) next
    
    meta <- map_dfr(basename(files), parse_naip_name) %>%
      mutate(file = files) %>%
      filter(!is.na(site))
    
    for (s in unique(meta$site)) {
      
      site_files <- meta %>%
        filter(site == s) %>%
        pull(file)
      
      if (length(site_files) == 0) next
      
      message("  averaging ", s, " (n = ", length(site_files), ")")
      
      ref <- rast(site_files[1])
      
      aligned <- lapply(site_files, function(f) {
        r <- rast(f)
        
        # check overlap first
        if (is.null(intersect(ext(r), ext(ref)))) {
          return(NULL)
        }
        
        r <- crop(r, ref, snap = "out")
        
        if (!compareGeom(r, ref, stopOnError = FALSE)) {
          r <- resample(r, ref, method = "bilinear")
        }
        
        r
      })
      
      aligned <- aligned[!sapply(aligned, is.null)]
      if (length(aligned) == 0) next
      
      r_stack <- rast(aligned)
      r_mean  <- app(r_stack, mean, na.rm = TRUE)
      
      names(r_mean) <- paste0(varname, "_mean")
      
      out_file <- file.path(
        out_dir,
        paste0(varname, "_mean_", s, "_", season, ".tif")
      )
      
      writeRaster(
        r_mean,
        out_file,
        overwrite = TRUE,
        wopt = list(gdal = c("COMPRESS=LZW"))
      )
    }
  }
}
#===============================================================================
# apply func to:
# NDVI
seasonal_site_mean(
  in_dir  = ndvi_dir,
  out_dir = out_ndvi_dir,
  pattern = "_NDVI\\.tif$",
  varname = "NDVI"
)

# LAI
seasonal_site_mean(
  in_dir  = lai_dir,
  out_dir = out_lai_dir,
  pattern = "\\.tif$",
  varname = "LAI"
)
