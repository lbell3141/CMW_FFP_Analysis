#load dir_dat_avg from driver_circles.R
library(dplyr)

mm_gpp <- dir_dat_avg[,c(1,2,7)]
data_chm <- data[,c(1:13)]
coi_df <- plot_df %>% select(contains(c("gpp", "CHM")))

#change df format for many rows instead of many column; change format to match gpp data
month_map <- c("jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6,
               "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)

mm_chm_df <- data_chm %>%
  pivot_longer(
    cols = ends_with("CHM"),
    names_to = c("month", ".value"),
    names_pattern = "(\\w+)_(\\w+)"
  ) %>%
  mutate(month = month_map[month],
         direction = str_sub(direction, 1, nchar(direction) - 1))

#combine dfs
combd_df <- merge(mm_gpp, mm_chm_df, by = c("month", "direction"))

combd_df <- combd_df %>%
  group_by(month) %>%
  mutate(
    zscore_gpp = (gpp - mean(gpp, na.rm = TRUE)) / sd(gpp, na.rm = TRUE),
    zscore_chm = (CHM - mean(CHM, na.rm = TRUE)) / sd(CHM, na.rm = TRUE)
  ) %>%
  ungroup() 

#Plot
ggplot(combd_df, aes(x = gpp, y = CHM, color = month)) +
  geom_point() +  # Plot the points
  geom_smooth(method = "lm", se = FALSE, aes(group = month)) +
  scale_color_viridis_c() +
  labs(x = "GPP", y = "CHM", color = "Month") +
  theme_minimal()


ggplot(combd_df, aes(x = zscore_gpp, y = zscore_chm, color = month)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "maroon")+
  scale_color_viridis_c() + 
  labs(x = "Canopy Height (Z-Score)", y = "GPP (Z-Score)", color = "Month") +
  theme_minimal()


