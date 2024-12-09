#NDVI scatter plot to show no correlation between directional NDVI and GPP
#Like the canpopy height and GPP scatter plot, normalize values first and then plot for all months 

#load dir_dat_avg and mm_dat from driver_circles.R
library(dplyr)
library(tidyr)
library(stringr)
#NDVI

mm_gpp <- dir_dat_avg[,c(1,2,7)]
data_ndvi <- mm_dat[,c(1, 26:37)]

month_map <- c("jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6,
               "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)

mm_ndvi_df <- data_ndvi %>%
  pivot_longer(
    cols = ends_with("NDVI"),
    names_to = c("month", ".value"),
    names_pattern = "(\\w+)_(\\w+)"
  ) %>%
  mutate(month = month_map[month],
         direction = str_sub(direction, 1, nchar(direction) - 1))
combd_df <- merge(mm_gpp, mm_ndvi_df, by = c("month", "direction"))

combd_df <- combd_df %>%
  group_by(month) %>%
  mutate(
    zscore_gpp = (gpp - mean(gpp, na.rm = TRUE)) / sd(gpp, na.rm = TRUE),
    zscore_NDVI = (NDVI - mean(NDVI, na.rm = TRUE)) / sd(NDVI, na.rm = TRUE)
  ) %>%
  ungroup() 


ggplot(combd_df, aes(x = gpp, y = NDVI, color = month)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = month)) +
  scale_color_viridis_c() +
  labs(x = "GPP", y = "NDVI", color = "Month") +
  theme_minimal()


ggplot(combd_df, aes(x = zscore_gpp, y = zscore_NDVI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "maroon")+
  scale_color_viridis_c() + 
  labs(x = "NDVI (Z-Score)", y = "GPP (Z-Score)") +
  theme_minimal()
