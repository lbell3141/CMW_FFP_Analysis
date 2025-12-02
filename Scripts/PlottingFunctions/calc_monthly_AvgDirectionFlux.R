calc_monthly_AvgDirectionFlux <- function(half_houly_dat){
  
  deg_int <- seq(0, 360, by = 20)
  deg_labels <- seq(20, 360, by = 20)
  
  half_houly_dat_grp <- half_houly_dat %>%
    dplyr::mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))
  
  dir_dat_avg <- half_houly_dat_grp %>%
    dplyr::group_by(mm, dir_group) %>%
    dplyr::summarise(
      gpp = mean(gpp, na.rm = TRUE),
      nee = mean(nee, na.rm = TRUE),
      reco = mean(reco, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(direction = as.numeric(as.character(dir_group))) %>%
    dplyr::select(mm, direction, gpp, nee, reco) %>%
    dplyr::filter(!is.na(direction))
  
  return(dir_dat_avg)
}
