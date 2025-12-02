#pull flux data from path (here, the SNOW serve) and process/filter for directional analysis
# half hourly data becomes 20 degree directional averages for each month


Load_FluxData <- function(SiteCode, root_path = "X:/moore/FluxNetData/") {
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
  matched_file <- csv_files[str_detect(basename(csv_files), "HH")]
  
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
      reco = RECO_DT_VUT_REF,
      nee = NEE_VUT_REF,
      precip = P_F,
      rel_h = RH,
      VPD = RHtoVPD(RH, TA_F, PA_F),
      ppfd = PPFD_IN,
      le = LE_F_MDS,
      swc = SWC_F_MDS_1
    ) %>%
    filter(HH_UTC %in% 8:17)
  
  return(dat_voi)
}
