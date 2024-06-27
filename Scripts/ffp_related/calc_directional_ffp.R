#calculate footprint climatologies by 10 degrees
#split_dat list from format_processed_data.R
#calc_footprint_FFP_climatology function from calc_footprint_FFP_climatology.R script (Kljun et al. 2015)

library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(terra)

#===============================================================================
#======================calculate directional footprint==========================
#===============================================================================
#list for FFP results
ffp_list <- list()

#create another function: pulls out variables instead of naming every time for ffp calculation
calc_ffp <- function(x) {
  calc_footprint_FFP_climatology(
    zm = x$zm, 
    z0 = x$z0, 
    umean = x$umean, 
    h = x$h, 
    ol = x$ol, 
    sigmav = x$sigmav,
    ustar = x$ustar,
    wind_dir = x$wind_dir, 
    fig = 1,
    domain = c(-200, 200, -200, 200), 
    r = seq(0, 90, by = 10)
  )
}

#use new function on list; calc ffp and store in list
ffp_list <- lapply(split_dat, calc_ffp)

#write list of calculations to data:
saveRDS(ffp_list, file = "calcd_ffp_list.rds")
#to read use readRDS()

#===============================================================================
#=========================calculate RAP cover value=============================
#===============================================================================
#load ffps and RAP data
ffp_list <- readRDS("./Data/calcd_ffp_list.rds")
rap <- rast("./Data/RAP/dif_rast.tif")

#split ffp_list into a list of x coords and y coords
x_list <- list()
y_list <- list()
for (i in seq_along(ffp_list)) {
  x_list[[i]] <- ffp_list[[i]]$xr
  y_list[[i]] <- ffp_list[[i]]$yr
}

#run code for ffp_contours_to_mask function in Countours_to_Mask.R
#apply function to lists and loaded RAP data:
rap_ffp_list <- list()
for (i in seq_along(x_list)) {
  rap_ffp_list[[i]] <- ffp_contours_to_mask(x_list[[i]], y_list[[i]], rap)
}

#convert to df
rap_ffp_df <- data.frame(direction = seq(10, 350, by = 10),
                         veg_cover = unlist(rap_ffp_list)
                         )
#direction window 40-50 missing
skip_directions <- seq(40, 50, by = 10)
rap_ffp_df <- rap_ffp_df[!rap_ffp_df$direction %in% skip_directions, ]

plot(rap_ffp_df$direction, rap_ffp_df$veg_cover)
