library(dplyr)
library(lubridate)
library(purrr)
library(plantecophys)
library(ggplot2)
library(viridis)
library(patchwork)
library(randomForest)


mod_mm_rf <- function(x) {
  if (nrow(x) < 50) return(NULL)
  
  rf_model <- randomForest(
    gpp ~ temp_atmos + swc + ppfd + rel_h + wind_sp + HH_UTC,
    data = x,
    importance = TRUE,
    ntree = 500
  )
  
  pred_df <- x %>%
    mutate(
      modeled_gpp = predict(rf_model, x),
      resid_gpp   = modeled_gpp - gpp
    )
  
  list(
    predictions = pred_df,
    model = rf_model
  )
}

summarize_by_dir <- function(pred_df) {
  pred_df %>%
    group_by(dir_group) %>%
    summarise(
      obs_avg_gpp   = mean(gpp, na.rm = TRUE),
      pred_avg_gpp  = mean(modeled_gpp, na.rm = TRUE),
      diff_avg_gpp  = mean(resid_gpp, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(direction = as.numeric(as.character(dir_group))) %>%
    arrange(direction)
}

deg_int    <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
#===============================================================================
#CMW============================================================================
#===============================================================================

cmwdat_file <- read.csv(
  "./SeriousStuff/Data/AMF_US-CMW_BASE_HH_2-5.csv",
  na.strings = "-9999",
  skip = 2
) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

cmwdat_voi <- cmwdat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm   = month(TIMESTAMP_START),
    doy  = yday(TIMESTAMP_START),
    day  = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM   = minute(TIMESTAMP_START),
    wind_sp = WS_1_1_1,
    temp_atmos = TA_1_1_1,
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

cmwdat_voi <- cmwdat_voi %>%
  mutate(
    dir_group = cut(
      wind_dir,
      breaks = deg_int,
      include.lowest = TRUE,
      labels = deg_labels
    )
  )

cmwdat_model <- cmwdat_voi %>%
  select(
    yyyy, mm, day, HH_UTC, MM, dir_group,
    gpp, temp_atmos, swc, ppfd, rel_h, wind_sp
  ) %>%
  na.omit()

# Split by year + month
cmwmm_split_dat <- split(
  cmwdat_model,
  list(cmwdat_model$yyyy, cmwdat_model$mm),
  drop = TRUE
)

#apply rf model
cmwmm_rf_results <- lapply(cmwmm_split_dat, mod_mm_rf)
cmwmm_rf_results <- cmwmm_rf_results[!sapply(cmwmm_rf_results, is.null)]

#summarize
cmwmm_mod_list <- lapply(
  cmwmm_rf_results,
  \(x) summarize_by_dir(x$predictions)
)

saveRDS(cmwmm_rf_results, "./SeriousStuff/Data/cmwmm_rf_results.RDS")
saveRDS(cmwmm_mod_list, "./SeriousStuff/Data/cmwmm_rf_summary_results.RDS")

#===============================================================================
#SRG============================================================================
#===============================================================================

srgdat_file <- read.csv(
  "./SeriousStuff/Data/AMF_US-SRG_FLUXNET_FULLSET_HH_2008-2024_5-7.csv",
  na.strings = "-9999",
  skip = 0
) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

srgdat_voi <- srgdat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm   = month(TIMESTAMP_START),
    doy  = yday(TIMESTAMP_START),
    day  = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM   = minute(TIMESTAMP_START),
    wind_sp = WS,
    temp_atmos = TA_F,
    u_star = USTAR,
    wind_dir = WD,
    gpp = GPP_DT_VUT_REF,
    precip = P,
    rel_h = RH,
    VPD = RHtoVPD(RH, TA_F, PA),
    ppfd = PPFD_IN,
    le = LE_F_MDS,
    swc = SWC_F_MDS_1
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

srgdat_voi <- srgdat_voi %>%
  mutate(
    dir_group = cut(
      wind_dir,
      breaks = deg_int,
      include.lowest = TRUE,
      labels = deg_labels
    )
  )

srgdat_model <- srgdat_voi %>%
  select(
    yyyy, mm, day, HH_UTC, MM, dir_group,
    gpp, temp_atmos, swc, ppfd, rel_h, wind_sp
  ) %>%
  na.omit()

# Split by year + month
srgmm_split_dat <- split(
  srgdat_model,
  list(srgdat_model$yyyy, srgdat_model$mm),
  drop = TRUE
)

#apply rf model
srgmm_rf_results <- lapply(srgmm_split_dat, mod_mm_rf)
srgmm_rf_results <- srgmm_rf_results[!sapply(srgmm_rf_results, is.null)]

#summarize
srgmm_mod_list <- lapply(
  srgmm_rf_results,
  \(x) summarize_by_dir(x$predictions)
)

saveRDS(srgmm_rf_results, "./SeriousStuff/Data/srgmm_rf_results.RDS")
saveRDS(srgmm_mod_list, "./SeriousStuff/Data/srgmm_rf_summary_results.RDS")

#===============================================================================
#SRM============================================================================
#===============================================================================

srmdat_file <- read.csv(
  "./SeriousStuff/Data/AMF_US-SRM_FLUXNET_FULLSET_HH_2004-2024_4-7.csv",
  na.strings = "-9999",
  skip = 0
) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

srmdat_voi <- srmdat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm   = month(TIMESTAMP_START),
    doy  = yday(TIMESTAMP_START),
    day  = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM   = minute(TIMESTAMP_START),
    wind_sp = WS,
    temp_atmos = TA_F,
    u_star = USTAR,
    wind_dir = WD,
    gpp = GPP_DT_VUT_REF,
    precip = P,
    rel_h = RH,
    VPD = RHtoVPD(RH, TA_F, PA),
    ppfd = PPFD_IN,
    le = LE_F_MDS,
    swc = SWC_F_MDS_1
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

srmdat_voi <- srmdat_voi %>%
  mutate(
    dir_group = cut(
      wind_dir,
      breaks = deg_int,
      include.lowest = TRUE,
      labels = deg_labels
    )
  )

srmdat_model <- srmdat_voi %>%
  select(
    yyyy, mm, day, HH_UTC, MM, dir_group,
    gpp, temp_atmos, swc, ppfd, rel_h, wind_sp
  ) %>%
  na.omit()

# Split by year + month
srmmm_split_dat <- split(
  srmdat_model,
  list(srmdat_model$yyyy, srmdat_model$mm),
  drop = TRUE
)

#apply rf model
srmmm_rf_results <- lapply(srmmm_split_dat, mod_mm_rf)
srmmm_rf_results <- srmmm_rf_results[!sapply(srmmm_rf_results, is.null)]

#summarize
srmmm_mod_list <- lapply(
  srmmm_rf_results,
  \(x) summarize_by_dir(x$predictions)
)

saveRDS(srmmm_rf_results, "./SeriousStuff/Data/srmmm_rf_results.RDS")
saveRDS(srmmm_mod_list, "./SeriousStuff/Data/srmmm_rf_summary_results.RDS")

#===============================================================================
#Wkg============================================================================
#===============================================================================


wkgdat_file <- read.csv(
  "./SeriousStuff/Data/AMF_US-Wkg_FLUXNET_FULLSET_HH_2004-2024_4-7.csv",
  na.strings = "-9999",
  skip = 0
) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

wkgdat_voi <- wkgdat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm   = month(TIMESTAMP_START),
    doy  = yday(TIMESTAMP_START),
    day  = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM   = minute(TIMESTAMP_START),
    wind_sp = WS,
    temp_atmos = TA_F,
    u_star = USTAR,
    wind_dir = WD,
    gpp = GPP_DT_VUT_REF,
    precip = P,
    rel_h = RH,
    VPD = RHtoVPD(RH, TA_F, PA),
    ppfd = PPFD_IN,
    le = LE_F_MDS,
    swc = SWC_F_MDS_1
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

wkgdat_voi <- wkgdat_voi %>%
  mutate(
    dir_group = cut(
      wind_dir,
      breaks = deg_int,
      include.lowest = TRUE,
      labels = deg_labels
    )
  )

wkgdat_model <- wkgdat_voi %>%
  select(
    yyyy, mm, day, HH_UTC, MM, dir_group,
    gpp, temp_atmos, swc, ppfd, rel_h, wind_sp
  ) %>%
  na.omit()

# Split by year + month
wkgmm_split_dat <- split(
  wkgdat_model,
  list(wkgdat_model$yyyy, wkgdat_model$mm),
  drop = TRUE
)

#apply rf model
wkgmm_rf_results <- lapply(wkgmm_split_dat, mod_mm_rf)
wkgmm_rf_results <- wkgmm_rf_results[!sapply(wkgmm_rf_results, is.null)]

#summarize
wkgmm_mod_list <- lapply(
  wkgmm_rf_results,
  \(x) summarize_by_dir(x$predictions)
)

saveRDS(wkgmm_rf_results, "./SeriousStuff/Data/wkgmm_rf_results.RDS")
saveRDS(wkgmm_mod_list, "./SeriousStuff/Data/wkgmm_rf_summary_results.RDS")
