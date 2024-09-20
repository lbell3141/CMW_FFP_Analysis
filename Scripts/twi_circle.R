#twi values around the tower (avg twi vals masked by annual avg footprint vals)

library(dplyr)

raster_frame <- readRDS("./Data/monthly_directional_ffps/raster_dataframes/FullRasterFrame.rds")
twi_frame <- raster_frame[,c(1,14:25)]
twi_frame$mean_twi <- rowMeans(twi_frame[, 2:13], na.rm = TRUE)

avg_twi <- twi_frame%>%
  select(direction, mean_twi)

#plot===========================================================================
ggplot(avg_twi) +  
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0, 5)), 
    color = "lightgrey"
  ) +
  geom_col(
    aes(x = direction, 
        y = 5,  # Set a constant height
        fill = mean_twi), 
    position = "dodge2"
  ) + 
  coord_polar() +
  scale_fill_gradientn(colors = viridis::viridis(256), 
                       name = "",
                       limits = c(0, 6), 
                       breaks = seq(0, 6, 2), 
                       labels = seq(0, 6, 2)) +
  theme(
    axis.title = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text.y = element_blank(), 
    axis.text.x = element_blank(),  
    legend.position = "bottom",
    text = element_text(color = "gray12", family = "Bell MT"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(title = "Average TWI")

library(terra)
twi <- rast("./Data/LiDAR/clipped_twi.tif")
ffp <- vect("./Data/LiDAR/repro_ffp.shp")
plot(twi, main = "")
plot(ffp, add = TRUE, border = "grey", lwd = 2, lty = 2)
