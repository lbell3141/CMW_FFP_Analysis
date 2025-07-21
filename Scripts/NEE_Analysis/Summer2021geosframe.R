library(terra)
library(dplyr)
library(tidyr)
library(stringr)

#===============================================================================
# Function to extract weighted raster value from FFP contours
#===============================================================================
ffp_contours_to_mask <- function(xr_list, yr_list, rast) {
  coord_pair_list <- list()
  latlon_list <- list()
  lines_list <- list()
  
  for (i in 1:length(xr_list)) {
    if (is.null(xr_list[[i]]) || is.null(yr_list[[i]])) {
      coord_pair_list[[i]] <- NA
      next
    }
    
    df <- data.frame(x = as.numeric(xr_list[[i]]), y = as.numeric(yr_list[[i]]))
    df <- na.omit(df)
    if (nrow(df) == 0) {
      coord_pair_list[[i]] <- NA
      next
    }
    coord_pair_list[[i]] <- as.matrix(df)
  }
  
  if (all(sapply(coord_pair_list, function(x) is.na(x)[1]))) return(NA)
  
  for (i in seq_along(coord_pair_list)) {
    if (is.na(coord_pair_list[[i]][1])) {
      latlon_list[[i]] <- NA
      next
    }
    lat <- (coord_pair_list[[i]][, 2] / 111111) + 31.6637
    lon <- (coord_pair_list[[i]][, 1] / (111111 * cos(31.6637 * pi / 180))) - 110.1777
    latlon_list[[i]] <- cbind(lon, lat)
  }
  
  for (i in seq_along(latlon_list)) {
    if (is.na(latlon_list[[i]][1])) {
      lines_list[[i]] <- NA
      next
    }
    lines_list[[i]] <- vect(latlon_list[[i]], type = "lines")
    crs(lines_list[[i]]) <- crs(rast)
  }
  
  valid_lines <- lines_list[!sapply(lines_list, function(x) is.na(x)[1])]
  if (length(valid_lines) < 10) return(NA)
  
  part_con_list <- list(valid_lines[[4]], valid_lines[[7]], valid_lines[[10]])
  
  masked_rast <- lapply(part_con_list, function(l) mask(rast, l))
  
  dif_con_list <- list(
    masked_rast[[1]],
    mask(masked_rast[[2]], masked_rast[[1]], inverse = TRUE),
    mask(masked_rast[[3]], masked_rast[[2]], inverse = TRUE)
  )
  
  rap_vals <- lapply(dif_con_list, function(r) {
    tryCatch(global(r, fun = mean, na.rm = TRUE), error = function(e) NA)
  })
  
  veg_cover <- 0.3 * sum(unlist(rap_vals[[1]])) +
    0.3 * sum(unlist(rap_vals[[2]])) +
    0.3 * sum(unlist(rap_vals[[3]]))
  
  return(veg_cover)
}

#===============================================================================
# Load raster data
#===============================================================================
ref_rast <- rast("./Data/RAP/avg_rast.tif")
twi <- rast("./Data/LiDAR/TWI/ReTWI.tif")
chm <- rast("./Data/LiDAR/CHM/reCHM.tif")
canopy <- chm > 2
canopy_num <- classify(canopy, cbind(1, 1), right = FALSE)
lai <- rast("./Data/LiDAR/LAI.tif")
lai <- project(lai, crs(ref_rast), method = "bilinear")

# Choose which raster to extract
raster_data_name <- "CHM"
rast_data <- chm  # or lai, or canopy_num

#===============================================================================
# Loop over FFP files and extract weighted values into a tidy dataframe
#===============================================================================
PathToMonthlyFootprintOutputs <- "./Data/monthly_directional_ffps/summer2021"
ffp_objs <- list.files(PathToMonthlyFootprintOutputs, pattern = "\\.rds$", full.names = TRUE)

# Dataframe to collect results
output_df <- data.frame()

for (file in ffp_objs) {
  ffp_list <- readRDS(file)
  x_list <- lapply(ffp_list, `[[`, "xr")
  y_list <- lapply(ffp_list, `[[`, "yr")
  ffp_names <- names(ffp_list)
  
  # Get month from file name
  month <- sub("_calcd_ffp_list.rds", "", basename(file))
  
  # Apply function and store results
  for (i in seq_along(x_list)) {
    val <- tryCatch({
      ffp_contours_to_mask(x_list[[i]], y_list[[i]], rast_data)
    }, error = function(e) NA)
    
    output_df <- rbind(output_df, data.frame(
      month = month,
      direction = ffp_names[i],
      value = val
    ))
  }
}

#===============================================================================
# Save final dataframe
#===============================================================================
saveRDS(output_df, file = paste0("./Data/monthly_directional_ffps/summer2021/testffps/ffp_", raster_data_name, "_values_df.rds"))

cover_df <- readRDS("./Data/monthly_directional_ffps/summer2021/testffps/ffp_Cover_values_df.RDS")%>%
  rename(Cover = value)
height_df <- readRDS("./Data/monthly_directional_ffps/summer2021/testffps/ffp_CHM_values_df.RDS")%>%
  rename(Height = value)
TWI_df <- readRDS("./Data/monthly_directional_ffps/summer2021/testffps/ffp_TWI_values_df.RDS")%>%
  rename(TWI = value)
LAI_df <- readRDS("./Data/monthly_directional_ffps/summer2021/testffps/ffp_LAI_values_df.RDS")%>%
  rename(LAI = value)
#NDVI_df <- readRDS("./Data/monthly_directional_ffps/summer2021/testffps/ffp_NDVI_values_df.RDS")%>%
#rename(NDVI = value)

df1 <- merge(cover_df, height_df, by = c("month", "direction"))
df2 <- merge(df1, TWI_df, by = c("month", "direction"))
full_geos_frame <- merge(df2, LAI_df, by = c("month", "direction"))
full_geos_frame$month <- tools::toTitleCase(full_geos_frame$month)

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
rf_pred <- readRDS("./Data/30minpredictionsRF.RDS")
rf_df <- bind_rows(rf_pred, .id = "mm")

gpp_dat <- dat_voi %>%
  select(yyyy, mm, gpp, dir_group) %>%
  rename(observed = gpp) %>%
  filter(!is.na(observed))

gpp_yr <- merge(rf_df, gpp_dat, by = c("mm", "observed", "dir_group")) %>%
  filter(yyyy == 2021, mm %in% 6:9) %>%
  mutate(residual = predicted - observed)%>%
  group_by(mm, dir_group)%>%
  reframe(yyyy = yyyy,
          mm = mm,
          observed_gpp = mean(observed, na.rm = T),
            predicted_gpp = mean(predicted, na.rm = T),
            residual_gpp = mean(residual, na.rm = T))%>%
  rename(direction = dir_group,
         month = mm)%>%
  distinct()
gpp_df <- gpp_yr %>%
  mutate(month = month.abb[as.numeric(month)])

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
#combine with geos data frame

full_summer_frame <- merge(gpp_df, full_geos_frame, by= c("month", "direction"))
saveRDS(full_summer_frame, file = paste0("./Data/monthly_directional_ffps/summer2021/testffps/full_summer_frame.rds"))


