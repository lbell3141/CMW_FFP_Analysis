#gpp and cover/height per month/contour
source("./Scripts/PlottingFunctions/calc_monthly_AvgDirectionFlux.R")

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
    #swc = SWC_1_1_1
  ) %>%
  filter(HH_UTC %in% 8:17)%>%
  filter(yyyy >= 2023)

avg_dat <- calc_monthly_AvgDirectionFlux(dat_voi)%>%
  filter(mm %in% 5:9)


library(ggplot2)
library(dplyr)

# Merge datasets by month and direction
merged_df <- all_months_df %>%
  inner_join(avg_dat %>% mutate(month = month.abb[mm]), by = c("month", "direction"))%>%
  filter(veg_cover_chm != 0)

#==============================
# Plot 1: GPP vs CHM-derived cover
#==============================
p_chm <- ggplot(merged_df, aes(x = veg_cover_chm, y = gpp)) +
  geom_point(color = "forestgreen", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  facet_wrap(~month) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Average Height (m)",
    y = "GPP",
    title = ""
  )

#==============================
# Plot 2: GPP vs Cover raster-derived vegetation
#==============================
p_cover <- ggplot(merged_df, aes(x = veg_cover_cover, y = gpp)) +
  geom_point(color = "goldenrod", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "orange3") +
  facet_wrap(~month) +
  theme_minimal(base_size = 14) +
  labs(
    x = "% Cover",
    y = "GPP",
    title = ""
  )

#==============================
# Display plots
#==============================
p_chm
p_cover
