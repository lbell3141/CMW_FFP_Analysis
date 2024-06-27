#calculate footprint climatologies by 10 degrees
#split_dat list from format_processed_data.R
#calc_footprint_FFP_climatology function from calc_footprint_FFP_climatology.R script (Kljun et al. 2015)

library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")

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