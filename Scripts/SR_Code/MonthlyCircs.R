source("./Scripts/PlottingFunctions/calc_monthly_AvgDirectionFlux.R")
source("./Scripts/PlottingFunctions/RawDat_plot_monthly_directional_gpp.R")
source("./Scripts/PlottingFunctions/RawDat_plot_monthly_directional_nee.R")
source("./Scripts/PlottingFunctions/plot_carbonflux_heatmaps.R")
source("./Scripts/PlottingFunctions/pred_GPP_RFmetmodel.R")
source("./Scripts/PlottingFunctions/plot_monthly_metmodelresiduals.R")

library(dplyr)
library(lubridate)
library(plantecophys)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)

#SRG============================================================================

csv_files <- list.files("./Data/RussHomework/GapfilledPartitionedFluxes_US-SRG_HH_ALL", pattern = "\\.csv$", full.names = TRUE)
fulldata <- do.call(rbind, lapply(csv_files, read.csv))

org_dat <- fulldata%>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

dat_voi <- org_dat %>%
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
    gpp = GPP,
    reco = RECO,
    nee = NEE,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN,
    le = LE,
    swc = SWC_1_1_1
  ) %>%
  filter(HH_UTC %in% 8:17)

pred_list <- pred_GPP_RFmetmodel(dat_voi)
plot_monthly_metmodel_residuals(pred_list)

mavg_flux <- calc_monthly_AvgDirectionFlux(dat_voi)

plot_monthly_directional_gpp(mavg_flux)
plot_monthly_directional_nee(mavg_flux)
plot_carbonflux_heatmaps(mavg_flux)

#SRM============================================================================

csv_files <- list.files("./Data/RussHomework/GapfilledPartitionedFluxes_US-SRM_HH_2018-2024", pattern = "\\.csv$", full.names = TRUE)
fulldata <- do.call(rbind, lapply(csv_files, read.csv))

org_dat <- fulldata%>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

dat_voi <- org_dat %>%
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
    gpp = GPP,
    reco = RECO,
    nee = NEE,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN,
    le = LE,
    swc = SWC_1_1_A
  ) %>%
  filter(HH_UTC %in% 8:17)

pred_list <- pred_GPP_RFmetmodel(dat_voi)
plot_monthly_metmodel_residuals(pred_list)

mavg_flux <- calc_monthly_AvgDirectionFlux(dat_voi)

plot_monthly_directional_gpp(mavg_flux)
plot_monthly_directional_nee(mavg_flux)
plot_carbonflux_heatmaps(mavg_flux)


#Wkg============================================================================

csv_file <- read.csv("X:/moore/FluxNetData/AMF_US-Wkg_FLUXNET_SUBSET_2004-2021_3-5 (1)/AMF_US-Wkg_FLUXNET_SUBSET_HH_2004-2021_3-5.csv")

org_dat <- csv_file%>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

dat_voi <- org_dat %>%
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

pred_list <- pred_GPP_RFmetmodel(dat_voi)
plot_monthly_metmodel_residuals(pred_list)

mavg_flux <- calc_monthly_AvgDirectionFlux(dat_voi)

plot_monthly_directional_gpp(mavg_flux)
plot_monthly_directional_nee(mavg_flux)
plot_carbonflux_heatmaps(mavg_flux)
