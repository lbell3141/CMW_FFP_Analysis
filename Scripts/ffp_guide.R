#code template for Kesondra 

#control-A/highlight all of calc_footprint_FFP_climatology.R and run the full 
#script to load the function into your environment 

#load packages 
library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(ggmap)
library(terra)

#define file path
PathToData <- "./Path/to/your/data.csv"

#load in prepared csv
footprint_data <- read.csv(PathToData)

#use function to calculate footprint
footprint_output <- calc_footprint_FFP_climatology(
  zm = footprint_data$zm, #instrument height above displacement height (displacement height is usually assumed to be 2/3 * canopy height when unknown)
  z0 = footprint_data$z0, #roughness length; I usually leave as NaN
  umean = footprint_data$umean, #average wind speed
  h = footprint_data$h, #boundary layer height. You can calculate this using an equation in Kljun's paper, but I usually just assume stable when unknown (h = 1000)
  ol = footprint_data$ol, #Obukhov length
  sigmav = footprint_data$sigmav, #sd of lateral wind speed fluctuation
  ustar = footprint_data$ustar, #friction velocity
  wind_dir = footprint_data$wind_dir, #wind direction in degrees
  domain = c(-1000, 1000, -1000, 1000), #plotting window. The footprint is calculated in meters from tower, so make sure your window is big enough to plot your full footprint
  #dx/dy refer to the resolution of the grid cells used to calculate footprint contribution. Default is 2m. If you have a small fooprint, you may wish to change for a finer scale analysis
  #or, to make the calculations run faster, you could decrease the resolution
  dx = 2, 
  dy = 2, 
  #nx/ny refer to the overall grid size. If you change dx/dy you'll need to change these proportionally to match
  nx = 1000, 
  ny = 1000, 
  r = seq(10, 90, 10), #how many contours you want. I like to specify a sequence of values but you can also give a single integer representing the upper bound. Most footprints go up to 80 or 90% (this model isn't valid past 90%)
  rslayer = 0, #forces a calculation for observations within roughness sublayer, but this is a very rough estimate, so I usually keep 0 and exclude these calculations
  smooth_data = 1, 
  crop = 1, #crop plot to size of last contour 
  pulse = 1, #shows calculation progress
  fig = 1 #plots figure at the end of the calculation
  ) 

#save your footprint (I just save mine as an R object)
saveRDS(footprint_output, file = "path/to/output/calculated_footprint_file.rds")

