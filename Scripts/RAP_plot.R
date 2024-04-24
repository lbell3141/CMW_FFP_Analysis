#fixing veg cover plot
#combining PFT classes to visualize proportion of woody vs herbaceous cover per footprint pixel
#data = RAP biomass product from 2001 - 2021

library(terra)
library(viridis)

pathtoRAPrasts <- "./Data/RAP"
pathtoFFPoutline <- "./Data/RAP/2017_FFP_Outline/twi_ffp_sec.shp"

#load rasters (and store names) from downloaded from GEE (RAP biomass product)
rast_files <- list.files(pathtoRAPrasts, pattern = "RAP_VegCover_\\d{4}\\.tif$", full.names = TRUE)
rast_names <- list.files(pathtoRAPrasts, pattern = "RAP_VegCover_\\d{4}\\.tif$", full.names = FALSE)

#apply names to rasters
rast_list <- lapply(rast_files, rast)
names(rast_list) <- rast_names

#make vector to store raster layer averages (change length with # of bands/PFTs)
avg_rast_list <- vector("list", length = 4)

#calculate average values for each layer (PFT) across the current record (2001-2021)
#loop: 1) Extract the same layer from each raster, 2) then calculate the average
for (i in 1:4) {
  layer_stack <- sds(lapply(rast_list, function(x) x[[i]]))
  avg_rast_list[[i]] <- app(layer_stack, mean)
}

#add herbaceous bands (1 and 2) and woody bands (3 and 4) together to create a 2 band raster
avg_rast <- rast(avg_rast_list)
combd_rast <- c(avg_rast[[1]] + avg_rast[[2]], avg_rast[[3]] + avg_rast[[4]])
names(combd_rast) <- c("Herbaceous", "Woody")

#create raster comparing proportion of woody/herbaceous cover per pixel
#woody - herbaceous
dif_rast <- combd_rast[[2]] - combd_rast[[1]]
ffp <- vect(pathtoFFPoutline)

#format plot
extent <- c(-110.18, -110.1755, 31.6615, 31.666)
dif_rast <- crop(dif_rast, extent)

#function for radially segmented lines
#dividing the radians of a circle 
#set line length 
#set line style
plot_lines <- function(center_x, center_y, num_lines, line_length) {
  angles <- seq(0, 2*pi, length.out = num_lines + 1)[-1]
  end_x <- center_x + line_length * cos(angles)
  end_y <- center_y + line_length * sin(angles)
  for (i in 1:num_lines) {
    lines(c(center_x, end_x[i]), c(center_y, end_y[i]), col = "black", lty = "dashed")
  }
}

#plot
plot(dif_rast, col = viridis(15), main = "Woody and Herbaceous Cover per Pixel at US-CMW")


#add ffp 90th extent shapefile
lines(ffp, col = "black", lwd = 2)

#add lines
center_x <- mean(extent[1:2])
center_y <- mean(extent[3:4])
num_lines <- 8  
line_length <- 0.1  
plot_lines(center_x, center_y, num_lines, line_length)


