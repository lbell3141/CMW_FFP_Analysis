#plotting elevation profile for CMW cross tower

elev_dat <- read.csv("./Data/LiDAR/TWI/elevation_profile.csv")
plot_elev <- elev_dat%>%
  select(-layer)%>%
  mutate(elevation = elevation - min(elevation))

ggplot(plot_elev, aes(x = distance, y = elevation))+
  geom_line(size = 1.2)+
  scale_y_continuous(breaks = seq(0,25,5), limits = c(0,25))+
  theme_minimal()+
  labs(
    title = "",
    x = "Distance (m)",
    y = "Elevation (m)")
