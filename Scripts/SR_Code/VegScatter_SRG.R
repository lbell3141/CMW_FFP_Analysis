#gpp and cover/height per month/contour
library(dplyr)
library(lubridate)
library(plantecophys)
source("./Scripts/PlottingFunctions/calc_monthly_AvgDirectionFlux.R")

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
#%>%
 # filter(yyyy >= 2023)

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



#===============================================================================
library(dplyr)
library(lubridate)
library(plantecophys)
library(ggplot2)

#-------------------------------------------------
# 1. Function: Monthly directional flux by YEAR
#-------------------------------------------------
calc_monthly_AvgDirectionFlux <- function(half_houly_dat){
  
  deg_int <- seq(0, 360, by = 20)
  deg_labels <- seq(20, 360, by = 20)
  
  half_houly_dat %>%
    mutate(
      dir_group = cut(wind_dir,
                      breaks = deg_int,
                      include.lowest = TRUE,
                      labels = seq(20, 360, by = 20))
    ) %>%
    group_by(yyyy, mm, dir_group) %>%
    summarize(
      gpp  = mean(gpp,  na.rm = TRUE),
      nee  = mean(nee,  na.rm = TRUE),
      reco = mean(reco, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      direction = as.numeric(as.character(dir_group)),
      month = month.abb[mm],
      year = yyyy
    ) %>%
    select(year, month, mm, direction, gpp, nee, reco) %>%
    filter(!is.na(direction))
}

#-------------------------------------------------
# 2. Load flux data
#-------------------------------------------------
csv_files <- list.files(
  "./Data/RussHomework/GapfilledPartitionedFluxes_US-SRG_HH_ALL",
  pattern = "\\.csv$", full.names = TRUE
)
fulldata <- do.call(rbind, lapply(csv_files, read.csv))

org_dat <- fulldata %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

#-------------------------------------------------
# 3. Extract variables
#-------------------------------------------------
dat_voi <- org_dat %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    wind_sp = WS_1_1_1,
    wind_dir = WD_1_1_1,
    gpp = GPP,
    nee = NEE,
    reco = RECO,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    swc = SWC_1_1_1
  ) %>%
  filter(HH_UTC %in% 8:17)

#-------------------------------------------------
# 4. Aggregate monthly directional flux (May–Sept)
#-------------------------------------------------
avg_dat <- calc_monthly_AvgDirectionFlux(dat_voi) %>%
  filter(mm %in% 5:9)

#-------------------------------------------------
# 5. Merge with vegetation cover
#-------------------------------------------------
merged_df <- all_months_df %>%
  inner_join(avg_dat, by = c("month", "direction")) %>%
  filter(veg_cover_chm != 0)

#-------------------------------------------------
# 6. Compute slope, p-value for each year × month
#-------------------------------------------------
lm_stats <- merged_df %>%
  group_by(year, month) %>%
  summarise(
    model = list(lm(gpp ~ veg_cover_chm)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    slope = coef(model)[2],
    pval  = summary(model)$coefficients[2, 4],  # p-value for slope
    sig   = ifelse(pval < 0.05, "*", "")        # annotate significant slopes
  ) %>%
  select(year, month, slope, sig) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = month.abb))

#-------------------------------------------------
# 7. Plot heatmap with significance annotation
#-------------------------------------------------
ggplot(lm_stats, aes(x = month, y = factor(year), fill = slope)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sig), color = "black", size = 6) +  # add * for significant
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Slope"
  ) +
  labs(
    x = "Month",
    y = "Year",
    title = "Slope: GPP ~ Veg Cover"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



