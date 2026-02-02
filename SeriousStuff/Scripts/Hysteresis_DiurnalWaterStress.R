#plot diurnal water stress hysteresis for each site

library(dplyr)
library(tidyr)
library(lubridate)
library(plantecophys)

# pull summer fluxes for sites--------------------------------------------------
deg_int    <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)

sites <- list(
  CMW = "./SeriousStuff/Data/AMF_US-CMW_BASE_HH_2-5.csv",
  SRM = "./SeriousStuff/Data/AMF_US-SRM_FLUXNET_FULLSET_HH_2004-2024_4-7.csv",
  SRG = "./SeriousStuff/Data/AMF_US-SRG_FLUXNET_FULLSET_HH_2008-2024_5-7.csv",
  Wkg = "./SeriousStuff/Data/AMF_US-Wkg_FLUXNET_FULLSET_HH_2004-2024_4-7.csv"
)

process_site <- function(file, site_name, nee_col, gpp_col, reco_col, temp_col, rh_col, wind_col, skip = 0) {
  df <- read.csv(file, na.strings = "-9999", skip = skip) %>%
    mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START))) %>%
    transmute(
      yyyy     = year(TIMESTAMP_START),
      mm       = month(TIMESTAMP_START),
      HH_UTC   = hour(TIMESTAMP_START),
      nee      = !!sym(nee_col),
      gpp      = !!sym(gpp_col),
      reco     = !!sym(reco_col),
      air_temp = !!sym(temp_col),
      rel_h    = !!sym(rh_col),
      wind_dir = !!sym(wind_col)
    ) %>%
#    filter(HH_UTC >= 8 & HH_UTC <= 17) %>%
    drop_na()
  
  # df_avg <- df %>%
  #   mutate(
  #     direction = as.numeric(as.character(
  #       cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels)
  #     )),
  #     site = site_name
  #   ) %>%
  #   group_by(site, direction) %>%
  #   summarise(
  #     nee  = mean(nee, na.rm = TRUE),
  #     gpp  = mean(gpp, na.rm = TRUE),
  #     reco = mean(reco, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # 
  # return(df_avg)
}

#pull site data with function
site_plot_dfs <- list(
  CMW = process_site(sites$CMW, "CMW", "NEE_PI", "GPP_PI", "RECO_PI", "TA_1_1_1", "RH_1_1_1", "WD_1_1_1", skip = 2),
  SRM = process_site(sites$SRM, "SRM", "NEE_VUT_REF", "GPP_DT_VUT_REF", "TA_F", "RH", "RECO_DT_VUT_REF", "WD"),
  SRG = process_site(sites$SRG, "SRG", "NEE_VUT_REF", "GPP_DT_VUT_REF", "TA_F", "RH", "RECO_DT_VUT_REF", "WD"),
  Wkg = process_site(sites$Wkg, "Wkg", "NEE_VUT_REF", "GPP_DT_VUT_REF", "TA_F", "RH", "RECO_DT_VUT_REF", "WD")
)

# all_plot_df <- bind_rows(site_plot_dfs)%>%
#   drop_na()




CMW <- site_plot_dfs$CMW
CMW_vpd <- CMW %>%
  mutate(
    vpd = RHtoVPD(RH = rel_h, TdegC = air_temp)  # VPD in kPa
  )
CMW_vpd_filt <- CMW_vpd %>%
  mutate(
    time_of_day = ifelse(HH_UTC < 12, "AM", "PM")
  )%>%
  filter(mm == 5)%>%
  group_by(HH_UTC)%>%
  summarise(gpp = mean(gpp, na.rm = T),
            air_temp = mean(air_temp, na.rm = T))%>%
  filter(HH_UTC %in% 8:17)


ggplot(CMW_vpd_filt, aes(x = air_temp, y = gpp, color = HH_UTC)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_viridis_c(option = "C", direction = -1) + 
  labs(
    x = "temp",
    y = "GPP",
    color = "Hour",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )

