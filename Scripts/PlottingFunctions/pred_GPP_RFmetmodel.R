#run random forest met model

pred_GPP_RFmetmodel <- function(half_hourly_dat, mon = 1:12) {
  library(tidyverse)
  library(lubridate)
  library(patchwork)
  library(randomForest)
  
  # Bin wind direction into 20° groups
  dat_model <- half_hourly_dat %>%
    mutate(
      dir_group = cut(wind_dir,
                      breaks = seq(0, 360, by = 20),
                      include.lowest = TRUE,
                      labels = seq(0, 340, by = 20)),
      direction = as.numeric(as.character(dir_group))
    ) %>%
    select(yyyy, mm, day, HH_UTC, MM, dir_group, gpp,
           temp_atmos, swc, ppfd, rel_h, wind_sp) %>%
    drop_na()
  
  # Filter to requested months before splitting
  dat_model <- dat_model %>% filter(mm %in% mon)
  
  # Split into monthly datasets
  mm_split <- split(dat_model, dat_model$mm)
  
  # Function to run RF + summarise residuals by direction
  mod_mm_rf <- function(df) {
    if (nrow(df) < 50) return(NULL)
    
    rf_model <- randomForest(
      gpp ~ temp_atmos + swc + ppfd + rel_h + wind_sp,
      data = df,
      importance = TRUE,
      ntree = 500
    )
    
    df %>%
      mutate(
        modeled_gpp = predict(rf_model, df),
        diff_gpp = modeled_gpp - gpp
      ) %>%
      filter(modeled_gpp > 0) %>%
      group_by(dir_group) %>%
      summarise(
        diff_avg_gpp = mean(diff_gpp, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(direction = as.numeric(as.character(dir_group))) %>%
      arrange(direction)
  }
  
  # Apply RF by month
  mm_results <- lapply(mm_split, mod_mm_rf)
  
  # Name results by correct month names
  month_numbers <- as.integer(names(mm_split))
  names(mm_results) <- month.name[month_numbers]
  
  return(mm_results)
}
