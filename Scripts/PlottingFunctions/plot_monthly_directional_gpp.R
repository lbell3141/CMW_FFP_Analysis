

plot_monthly_directional_gpp <- function(SiteCode, TemporalResolution, root_path = "X:/moore/FluxNetData/") {
  # Load necessary packages
  library(dplyr)
  library(lubridate)
  library(plantecophys)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  
  #Pull file from SNOW server
  site_folder <- list.dirs(root_path, recursive = FALSE, full.names = TRUE) %>%
    keep(~ str_detect(basename(.x), SiteCode))
  
  if (length(site_folder) == 0) stop("No folder found for SiteCode")
  
  csv_files <- list.files(site_folder, pattern = "\\.csv$", full.names = TRUE)
  matched_file <- csv_files[str_detect(basename(csv_files), TemporalResolution)]
  
  if (length(matched_file) == 0) stop("No matching file found for TemporalResolution")
  
  matched_file <- matched_file[1]
  
  #load dataframe
  dat_file <- read.csv(matched_file, na.strings = "-9999") %>%
    mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))
  
  dat_voi <- dat_file %>%
    transmute(
      yyyy = year(TIMESTAMP_START),
      mm = month(TIMESTAMP_START),
      doy = yday(TIMESTAMP_START),
      day = day(TIMESTAMP_START),
      HH_UTC = hour(TIMESTAMP_START),
      MM = minute(TIMESTAMP_START),
      wind_sp = WS_F,
      temp_atmos = TA_F,
      u_star = USTAR,
      wind_dir = WD,
      gpp = GPP_DT_VUT_REF,
      precip = P_F,
      rel_h = RH,
      VPD = RHtoVPD(RH, TA_F, PA_F),
      ppfd = PPFD_IN,
      le = LE_F_MDS,
      swc = SWC_F_MDS_1
    ) %>%
    filter(HH_UTC %in% 8:17)
  
  #group fluxes 
  deg_int <- seq(0, 360, by = 20)
  deg_labels <- seq(20, 360, by = 20)
  
  dat_voi <- dat_voi %>%
    mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))
  
  #calculate average fluxes by direction
  dir_dat_avg <- dat_voi %>%
    group_by(mm, dir_group) %>%
    summarise(gpp = mean(gpp, na.rm = TRUE), .groups = "drop") %>%
    mutate(direction = as.numeric(as.character(dir_group))) %>%
    select(mm, direction, gpp)
  
  # Prep dataframe from plotting
  month_split <- split(dir_dat_avg, dir_dat_avg$mm)
  month_abbr <- month.abb[1:12]
  
  month_split <- map2(month_split, month_abbr, function(df, abbr) {
    df %>%
      rename_with(~ ifelse(.x %in% c("mm", "direction"), .x, paste0(abbr, "_", .x))) %>%
      select(-mm)
  })
  
  plot_df <- reduce(month_split, full_join, by = "direction")
  plot_df <- plot_df[1:18, ]
  
  #Loop through and create monthly plots
  create_monthly_gpp_plot <- function(month_abbr, month_full) {
    variable_col <- paste0(month_abbr, "_gpp")
    
    min_value <- min(plot_df[[variable_col]], na.rm = TRUE)
    max_value <- max(plot_df[[variable_col]], na.rm = TRUE)
    
    ggplot(plot_df, aes(x = direction, y = 5, fill = !!sym(variable_col))) +
      geom_col(width = 20) +
      coord_polar() +
      scale_fill_viridis_c(option = "viridis", limits = c(min_value, max_value),
                           breaks = c(min_value, max_value),
                           labels = c(sprintf("%.1f", min_value), sprintf("%.1f", max_value)),
                           name = NULL) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)
      ) +
      labs(title = month_full) +
      guides(fill = guide_colorbar(barwidth = 3, barheight = 0.5))
  }
  
  # Apply loop
  month_full <- month.name[1:12]
  plots <- Map(create_monthly_gpp_plot, month_abbr, month_full)
  
  final_plot <- wrap_plots(plots, ncol = 6) &
    theme(legend.position = "bottom")
  
  GPP_plot <- final_plot + plot_annotation(title = "Average Directional GPP (µmol CO₂ m⁻² s⁻¹)")
  return(GPP_plot)
}
