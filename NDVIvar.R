#Calculate NDVI from a list of rasters
library(stringr)
library(terra)
library(dplyr)
library(ggplot2)
library(spdep)

PathToRawRasts <- "./Data_NDVIvar/RAW"

find_files <- function(folder) {
  #list all files in folder, set pattern, create object with files that match the pattern
  all_files <- list.files(folder, full.names = T)
  pattern <- ".tif"
  files_oi <- all_files[str_detect(all_files, pattern)]
  return(files_oi)
}
PSStifs <- find_files(PathToRawRasts)
NDVI_list <- list()
for (i in seq_along(PSStifs)){
  #reading in raster
  PSSrast <- rast(PSStifs[i])
  #calculating NDVI
  NDVI_list[[i]] <- (PSSrast[[4]] - PSSrast[[3]]) / (PSSrast[[4]] + PSSrast[[3]])
}

site_name <- c("CMW", "SRG", "SRM", "WHS", "WKG")

#convert to ggplot instead to solve common legend problem
NDVI_dat <- lapply(seq_along(NDVI_list), function(i){
  rast_df <- as.data.frame(NDVI_list[[i]], xy = T)%>%
    na.omit()%>%
    mutate(source = paste0(site_name[i]))
  return(rast_df)
})%>%
  bind_rows()%>%
  rename(ndvi = nir)
#head(NDVI_dat)

ggplot(NDVI_dat, aes(x = x, y = y, fill = ndvi)) +
  geom_tile() +
  scale_fill_viridis_c(name = "NDVI") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(color = "gray12", family = "Bell MT"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank()
  ) +
  facet_wrap(~source, scales = "free") +
  labs(title = "Mid-August 2024 NDVI", x = "Longitude", y = "Latitude")


#calculate variance of each raster
ndvi_var <- sapply(NDVI_list, function(x) {
  #extract rast values for calc
  values_x <- values(x)
  var(values_x, na.rm = T)
  })

names(ndvi_var) <- tools::file_path_sans_ext(basename(PSStifs))
print(ndvi_var)
par(mfrow = c(2, 3)) 
 for (i in seq_along(NDVI_list)) {
  #remove .tif from plot names
  file_name <- tools::file_path_sans_ext(basename(PSStifs[i]))
  plot(NDVI_list[[i]], main = paste(file_name, "NDVI"), col = viridis::viridis(100))
  mtext(side = 1, line = 0.1, adj = 0, cex = 0.8, 
        text = paste("Variance:", round(ndvi_var[i], 2)))
}


#spatial variance: identifies clusters of high and low values relative to the mean
#convert back to list (as dfs though, not as raster list)
NDVI_dat <- lapply(NDVI_list, function(raster) {
  as.data.frame(raster, xy = TRUE) %>% na.omit()
})
#calc getis stat
calculate_getis_ord <- function(j) {
  coords <- j[, c("x", "y")]
  ndvi_values <- j$ndvi
  #create neighbor object class
  nb <- dnearneigh(coords, 0, max(dist(coords)))
  #specify weights using nb; w = row standardized  
  lw <- nb2listw(nb, style = "W")
  #calc stat
  local_g <- localG(ndvi_values, lw)
  j$g_stat <- local_g
  
  return(j)
}
#NDVI_dat <- NDVI_dat[1]
getis_results <- lapply(NDVI_dat, calculate_getis_ord)
ggplot(getis_results[[1]], aes(x = x, y = y, fill = g_stat)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Getis-Ord G") +
  theme_minimal() +
  labs(title = "NDVI Hotspots")
