#plot relationships of drivers and GPP for June - September

# Load libraries
library(dplyr)
library(lubridate)
library(plantecophys)
library(tidyr)
library(ggplot2)
library(patchwork)
library(randomForest)

# Load data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create primary dataframe
dat_voi <- dat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    wind_sp = WS_1_1_1,
    temp_atmos = TA_1_1_1,
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    nee = NEE_PI,
    reco = RECO_PI,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))

dat_voi <- dat_voi %>%
  mutate(month_label = factor(mm, levels = 6:9, labels = c("June", "July", "August", "September")))

dat_long <- dat_voi %>%
  pivot_longer(cols = c(swc, temp_atmos, rel_h, ppfd, wind_sp),
               names_to = "x_var",
               values_to = "x_value")

ggplot(dat_long, aes(x = x_value, y = gpp)) +
  geom_point(alpha = 0.3, size = 0.8, color = "#1f78b4") +
  #geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  facet_grid(month_label ~ x_var, scales = "free_x") +
  labs(
    title = "",
    x = "Drivers",
    y = "GPP"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#===============================================================================
#binning 
#===============================================================================
dat_long <- dat_voi %>%
  pivot_longer(cols = c(swc, temp_atmos, rel_h, ppfd, wind_sp),
               names_to = "x_var",
               values_to = "x_value")

# var_medians <- dat_long %>%
#   group_by(month_label, x_var) %>%
#   summarise(
#     bin_lower = quantile(x_value, 0.4, na.rm = TRUE),
#     median_x  = median(x_value, na.rm = TRUE),
#     bin_upper = quantile(x_value, 0.6, na.rm = TRUE),
#     .groups = "drop"
#   )

var_medians <- dat_long %>%
  group_by(x_var) %>%
  summarise(
    bin_lower = quantile(x_value, 0.35, na.rm = TRUE),
    median_x  = median(x_value, na.rm = TRUE),
    bin_upper = quantile(x_value, 0.65, na.rm = TRUE),
    .groups = "drop"
  )


filter_except_one <- function(dat_long, var_medians, free_var) {
  
  filtered_bins <- var_medians %>%
    filter(x_var != free_var)
  
  dat_filtered <- dat_long %>%
    filter(x_var != free_var) %>%
    left_join(filtered_bins, by = "x_var") %>%
    filter(x_value >= bin_lower & x_value <= bin_upper)
  
  dat_passed <- dat_filtered %>%
    group_by(yyyy, mm, day, HH_UTC, MM) %>%
    summarise(n_vars = n_distinct(x_var), .groups = "drop") %>%
    filter(n_vars == length(unique(filtered_bins$x_var)))
  
  dat_long %>%
    inner_join(dat_passed, by = c("yyyy", "mm", "day", "HH_UTC", "MM")) %>%
    filter(x_var == free_var)
}



dat_vary_temp <- filter_except_one(dat_long, var_medians, "temp_atmos")
dat_vary_rel_h <- filter_except_one(dat_long, var_medians, "rel_h")
dat_vary_wind_sp <- filter_except_one(dat_long, var_medians, "wind_sp")
dat_vary_swc <- filter_except_one(dat_long, var_medians, "swc")
dat_vary_ppfd <- filter_except_one(dat_long, var_medians, "ppfd")


p1 <- ggplot(dat_vary_temp, aes(x = x_value, y = gpp)) +
  geom_point(alpha = 0.3) +
  #geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  #facet_wrap(~ month_label) +
  labs(
    x = "Temperature",  # you can customize this
    y = "GPP",
    title = ""
  ) +
  theme_minimal(base_size = 14)

p2 <- ggplot(dat_vary_rel_h, aes(x = x_value, y = gpp)) +
  geom_point(alpha = 0.3) +
  #geom_smooth(method = "lm", color = "darkred", se = FALSE) +
 # facet_wrap(~ month_label) +
  labs(
    x = "Relative Humidity",
    y = "GPP",
    title = ""
  ) +
  theme_minimal(base_size = 14)

p3 <- ggplot(dat_vary_wind_sp, aes(x = x_value, y = gpp)) +
  geom_point(alpha = 0.3) +
  #geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  #facet_wrap(~ month_label) +
  labs(
    x = "Wind Speed",
    y = "GPP",
    title = ""
  ) +
  theme_minimal(base_size = 14)

p4 <- ggplot(dat_vary_swc, aes(x = x_value, y = gpp)) +
  geom_point(alpha = 0.3) +
  #geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  #facet_wrap(~ month_label) +
  labs(
    x = "Soil Water Content",
    y = "GPP",
    title = ""
  ) +
  theme_minimal(base_size = 14)

p5 <- ggplot(dat_vary_ppfd, aes(x = x_value, y = gpp)) +
  geom_point(alpha = 0.3) +
  #geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  #facet_wrap(~ month_label) +
  labs(
    x = "PPFD",  # you can customize this
    y = "GPP",
    title = ""
  ) +
  theme_minimal(base_size = 14)

(p1+ p2+ p3)/(p4+p5 + plot_spacer())
#===============================================================================
#random forest
#===============================================================================
dat_model <- dat_voi%>%
  select(yyyy, mm, day, HH_UTC, MM, dir_group, gpp, temp_atmos, swc, ppfd, rel_h, wind_sp) %>%
  na.omit()#%>%
  filter(mm %in% 6:9)

set.seed(123)

rf_model <- randomForest(
  gpp ~ temp_atmos + swc + ppfd + rel_h + wind_sp,
  data = dat_model,
  importance = TRUE,
  ntree = 500 
)
#saveRDS(rf_model, "./Data/fullyear30minRF.RDS")
print(rf_model)

rf_model$rsq[500]

varImpPlot(rf_model, type = 1, main = "Variable Importance (Mean Decrease in Accuracy)")
partialPlot(rf_model, dat_model, "temp_atmos", main = "Effect of Temperature on GPP")
partialPlot(rf_model, dat_model, "ppfd", main = "Effect of PPFD on GPP")


mod_plot_dat <- dat_model %>%
  mutate(pred_gpp = rf_model$predicted) %>%
  group_by(mm, dir_group) %>%
  summarise(across(c(gpp, pred_gpp), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(diff_gpp = pred_gpp -gpp)

plot_input <- mod_plot_dat %>%
  rename(direction = dir_group, diff_avg_gpp = diff_gpp)
mm_split_dat <- split(plot_input, plot_input$mm)

all_diff_avg_gpp <- unlist(lapply(mm_split_dat, \(df) df$diff_avg_gpp))
overall_min <- min(all_diff_avg_gpp, na.rm = TRUE)
overall_max <- max(all_diff_avg_gpp, na.rm = TRUE)


plot_gpp_difference <- function(df, month_num) {
  df <- df %>%
    mutate(direction = factor(direction, levels = seq(0, 340, by = 20)))  # ensures proper spacing
  
  ggplot(df) +
    geom_col(
      aes(x = direction, y = 5, fill = diff_avg_gpp),
      width = 1  # one full bar per factor level
    ) +
    coord_polar() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      limits = c(overall_min, overall_max),
      name = "GPP Residual"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.position = "bottom",
      text = element_text(color = "black"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12)
    ) +
    guides(fill = guide_colorbar(barwidth = 6, barheight = 1)) +
    labs(title = month.name[month_num])
}

plot_list <- Map(plot_gpp_difference, mm_split_dat, as.integer(names(mm_split_dat)))

final_plot <- wrap_plots(plot_list, ncol = 6) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot +
  plot_annotation(title = "Directional Residual GPP (Predicted - Observed)") +
  theme(text = element_text(color = "black"))




#===============================================================================
#random forest-- apply by month
#===============================================================================
dat_model <- dat_voi%>%
  select(yyyy, mm, day, HH_UTC, MM, dir_group, gpp, temp_atmos, swc, ppfd, rel_h, wind_sp) %>%
  na.omit()
mm_split_dat <- split(dat_model, dat_model$mm)

mod_mm_rf <- function(x) {
  if (nrow(x) < 50) return(NULL)
  
  rf_model <- randomForest(
    gpp ~ temp_atmos + swc + ppfd + rel_h + wind_sp,
    data = x,
    importance = TRUE,
    ntree = 500
  )
  
  summary_df <- x %>%
    mutate(
      modeled_gpp = predict(rf_model, x),
      diff_gpp = modeled_gpp - gpp
    ) %>%
    filter(modeled_gpp > 0) %>%
    group_by(dir_group) %>%
    summarise(
      mod_avg_gpp = mean(modeled_gpp, na.rm = TRUE),
      real_avg_gpp = mean(gpp, na.rm = TRUE),
      diff_avg_gpp = mean(diff_gpp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(direction = as.numeric(as.character(dir_group))) %>%
    arrange(direction)
  
  list(summary = summary_df, model = rf_model)
}

mm_rf_results <- lapply(mm_split_dat, mod_mm_rf)

mm_mod_list <- lapply(mm_rf_results, \(x) x$summary)
saveRDS(mm_rf_results, "./Data/mm_rf_results.RDS")

all_diff_avg_gpp <- unlist(lapply(mm_mod_list, \(df) df$diff_avg_gpp))
overall_min <- min(all_diff_avg_gpp, na.rm = TRUE)
overall_max <- max(all_diff_avg_gpp, na.rm = TRUE)



plot_gpp_difference <- function(df, month_num) {
  df <- df %>%
    mutate(direction = factor(direction, levels = seq(0, 340, by = 20)))  # ensures equal spacing
  
  ggplot(df) +
    geom_col(
      aes(x = direction, y = 5, fill = diff_avg_gpp),
      width = 1
    ) +
    coord_polar() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      limits = c(overall_min, overall_max),
      name = "Residual GPP"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.position = "bottom",
      text = element_text(color = "black"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12)
    ) +
    guides(fill = guide_colorbar(barwidth = 6, barheight = 1)) +
    labs(title = month.name[month_num])
}
# Get numeric month values from names of mm_mod_list
plot_list <- Map(plot_gpp_difference, mm_mod_list, as.integer(names(mm_mod_list)))

final_plot <- wrap_plots(plot_list, ncol = 6) +  # Adjust ncol as needed
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot +
  plot_annotation(title = "Directional Residual GPP (Predicted - Observed)") +
  theme(text = element_text(color = "black"))

