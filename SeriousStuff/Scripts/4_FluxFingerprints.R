library(dplyr)
library(lubridate)
library(plantecophys)
library(stringr)
library(purrr)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(viridis)

deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)

#===============================================================================
#Dataframe Prep
#===============================================================================

# Load data
cmwdat_file <- read.csv("./SeriousStuff/Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# prep frame
cmwdat_voi <- cmwdat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    nee = NEE_PI,
    wind_dir = WD_1_1_1) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  drop_na()

cmwdir_dat_avg <- cmwdat_voi %>%
  mutate(
    dir_group = cut(
      wind_dir,
      breaks = deg_int,
      include.lowest = TRUE,
      labels = deg_labels
    ),
    direction = as.numeric(as.character(dir_group))
  ) %>%
  group_by(yyyy, mm, direction) %>%
  summarise(
    nee = mean(nee, na.rm = TRUE),
    .groups = "drop"
  )

#rank nee within each month/year
cmwranked_df <- cmwdir_dat_avg %>%
  group_by(yyyy, mm) %>%
  mutate(
    nee_rank = rank(nee, ties.method = "average", na.last = "keep")
  ) %>%
  ungroup() %>%
  mutate(
    ym = sprintf("%d-%02d", yyyy, mm),
    ym = factor(ym, levels = unique(ym[order(yyyy, mm)])),
    direction = factor(direction)
  )

cmwyear_breaks <- cmwranked_df %>%
  distinct(yyyy, ym) %>%
  group_by(yyyy) %>%
  summarise(
    first_ym = first(ym),
    .groups = "drop"
  )

# Plot
cmwyear_labels <- cmwranked_df %>%
  distinct(yyyy, ym) %>%
  group_by(yyyy) %>%
  summarise(
    ym_year = first(ym),
    .groups = "drop"
  )

cmw <- ggplot(cmwranked_df, aes(
  x = direction,
  y = ym,
  fill = nee_rank
)) +
  geom_tile(color = NA) +
  geom_hline(
    data = cmwyear_breaks,
    aes(yintercept = as.numeric(first_ym) - 0.5),
    color = "black",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis(
    option = "viridis",
    name = "monthly NEE rank",
    direction = -1
  ) +
  scale_x_discrete(
    breaks = levels(cmwranked_df$direction)[seq(1, length(levels(cmwranked_df$direction)), by = 2)]
  ) +
  scale_y_discrete(
    breaks = cmwyear_labels$ym_year,
    labels = cmwyear_labels$yyyy
  ) +
  labs(
    x = "Wind direction (°)",
    y = "Year",
    title = "CMW"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    legend.position = "none"
  )

#SRG===============================================================================

# Load data
srgdat_file <- read.csv("./SeriousStuff/Data/AMF_US-SRG_FLUXNET_FULLSET_HH_2008-2024_5-7.csv", na.strings = "-9999", skip = 0) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create primary dataframe
srgdat_voi <- srgdat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    nee = NEE_VUT_REF,
    wind_dir = WD) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  drop_na()

srgdir_dat_avg <- srgdat_voi %>%
  mutate(
    dir_group = cut(
      wind_dir,
      breaks = deg_int,
      include.lowest = TRUE,
      labels = deg_labels
    ),
    direction = as.numeric(as.character(dir_group))
  ) %>%
  group_by(yyyy, mm, direction) %>%
  summarise(
    nee = mean(nee, na.rm = TRUE),
    .groups = "drop"
  )

# Rank nee within each month/year
srgranked_df <- srgdir_dat_avg %>%
  group_by(yyyy, mm) %>%
  mutate(
    nee_rank = rank(nee, ties.method = "average", na.last = "keep")
  ) %>%
  ungroup() %>%
  mutate(
    ym = sprintf("%d-%02d", yyyy, mm),
    ym = factor(ym, levels = unique(ym[order(yyyy, mm)])),
    direction = factor(direction)
  )

srgyear_breaks <- srgranked_df %>%
  distinct(yyyy, ym) %>%
  group_by(yyyy) %>%
  summarise(
    first_ym = first(ym),
    .groups = "drop"
  )

#Plot
srgyear_labels <- srgranked_df %>%
  distinct(yyyy, ym) %>%
  group_by(yyyy) %>%
  summarise(
    ym_year = first(ym),
    .groups = "drop"
  )

srg <- ggplot(srgranked_df, aes(
  x = direction,
  y = ym,
  fill = nee_rank
)) +
  geom_tile(color = NA) +
  geom_hline(
    data = srgyear_breaks,
    aes(yintercept = as.numeric(first_ym) - 0.5),
    color = "black",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis(
    option = "viridis",
    name = "monthly NEE rank",
    direction = -1
  ) +
  scale_x_discrete(
    breaks = levels(srgranked_df$direction)[seq(1, length(levels(srgranked_df$direction)), by = 2)]
  ) +
  scale_y_discrete(
    breaks = srgyear_labels$ym_year,
    labels = srgyear_labels$yyyy
  ) +
  labs(
    x = "Wind direction (°)",
    y = "",
    title = "SRG"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    legend.position = "none"
  )


#SRM===============================================================================

# Load data
srmdat_file <- read.csv("./SeriousStuff/Data/AMF_US-SRM_FLUXNET_FULLSET_HH_2004-2024_4-7.csv", na.strings = "-9999", skip = 0) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create primary dataframe
srmdat_voi <- srmdat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    nee = NEE_VUT_REF,
    wind_dir = WD) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  drop_na()

srmdir_dat_avg <- srmdat_voi %>%
  mutate(
    dir_group = cut(
      wind_dir,
      breaks = deg_int,
      include.lowest = TRUE,
      labels = deg_labels
    ),
    direction = as.numeric(as.character(dir_group))
  ) %>%
  group_by(yyyy, mm, direction) %>%
  summarise(
    nee = mean(nee, na.rm = TRUE),
    .groups = "drop"
  )

# Rank nee within each month/year
srmranked_df <- srmdir_dat_avg %>%
  group_by(yyyy, mm) %>%
  mutate(
    nee_rank = rank(nee, ties.method = "average", na.last = "keep")
  ) %>%
  ungroup() %>%
  mutate(
    ym = sprintf("%d-%02d", yyyy, mm),
    ym = factor(ym, levels = unique(ym[order(yyyy, mm)])),
    direction = factor(direction)
  )

srmyear_breaks <- srmranked_df %>%
  distinct(yyyy, ym) %>%
  group_by(yyyy) %>%
  summarise(
    first_ym = first(ym),
    .groups = "drop"
  )

#Plot
srmyear_labels <- srmranked_df %>%
  distinct(yyyy, ym) %>%
  group_by(yyyy) %>%
  summarise(
    ym_year = first(ym),
    .groups = "drop"
  )

srm <- ggplot(srmranked_df, aes(
  x = direction,
  y = ym,
  fill = nee_rank
)) +
  geom_tile(color = NA) +
  geom_hline(
    data = srmyear_breaks,
    aes(yintercept = as.numeric(first_ym) - 0.5),
    color = "black",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis(
    option = "viridis",
    name = "monthly NEE rank",
    direction = -1
  ) +
  scale_x_discrete(
    breaks = levels(srmranked_df$direction)[seq(1, length(levels(srmranked_df$direction)), by = 2)]
  ) +
  scale_y_discrete(
    breaks = srmyear_labels$ym_year,
    labels = srmyear_labels$yyyy
  ) +
  labs(
    x = "Wind direction (°)",
    y = "",
    title = "SRM"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    legend.position = "none"
  )

#Wkg===============================================================================

# Load data
wkgdat_file <- read.csv("./SeriousStuff/Data/AMF_US-Wkg_FLUXNET_FULLSET_HH_2004-2024_4-7.csv", na.strings = "-9999", skip = 0) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create primary dataframe
wkgdat_voi <- wkgdat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    nee = NEE_VUT_REF,
    wind_dir = WD) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  drop_na()

wkgdir_dat_avg <- wkgdat_voi %>%
  mutate(
    dir_group = cut(
      wind_dir,
      breaks = deg_int,
      include.lowest = TRUE,
      labels = deg_labels
    ),
    direction = as.numeric(as.character(dir_group))
  ) %>%
  group_by(yyyy, mm, direction) %>%
  summarise(
    nee = mean(nee, na.rm = TRUE),
    .groups = "drop"
  )

# Rank nee within each month/year
wkgranked_df <- wkgdir_dat_avg %>%
  group_by(yyyy, mm) %>%
  mutate(
    nee_rank = rank(nee, ties.method = "average", na.last = "keep")
  ) %>%
  ungroup() %>%
  mutate(
    ym = sprintf("%d-%02d", yyyy, mm),
    ym = factor(ym, levels = unique(ym[order(yyyy, mm)])),
    direction = factor(direction)
  )%>%
  drop_na()

wkgyear_breaks <- wkgranked_df %>%
  distinct(yyyy, ym) %>%
  group_by(yyyy) %>%
  summarise(
    first_ym = first(ym),
    .groups = "drop"
  )

#Plot
wkgyear_labels <- wkgranked_df %>%
  distinct(yyyy, ym) %>%
  group_by(yyyy) %>%
  summarise(
    ym_year = first(ym),
    .groups = "drop"
  )

wkg <- ggplot(wkgranked_df, aes(
  x = direction,
  y = ym,
  fill = nee_rank
)) +
  geom_tile(color = NA) +
  geom_hline(
    data = wkgyear_breaks,
    aes(yintercept = as.numeric(first_ym) - 0.5),
    color = "black",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis(
    option = "viridis",
    name = "Monthly NEE Ranks",
    direction = -1,
    limits = c(1, 18),
    breaks = c(1, 18),
    labels = c("1", "18")
  ) +
  scale_x_discrete(
    breaks = levels(wkgranked_df$direction)[seq(1, length(levels(wkgranked_df$direction)), by = 2)]
  ) +
  scale_y_discrete(
    breaks = wkgyear_labels$ym_year,
    labels = wkgyear_labels$yyyy
  ) +
  labs(
    x = "Wind direction (°)",
    y = "",
    title = "Wkg"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9)
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "bottom",
      title.hjust = 0.5
    )
  )


#combine plots------------------------------------------------------------------
combined <- (cmw + srm + srg + wkg) +
  plot_layout(ncol = 4, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
combined

#===============================================================================
#===============================================================================
#===============================================================================
#Create frequency plots of high values==========================================
  `#supplement interannual patterns in rankings by showing the frequency of 
    #high ranks (1-6) by direction for all sites

end_rank <- 6

cmw_freq <- cmwranked_df %>%
  group_by(direction) %>%
  summarise(
    high_rank_count = sum(nee_rank < end_rank, na.rm = T),
    total_obs = n(),
    .groups = "drop"
  )%>%
  mutate(perc_highrank = (high_rank_count/total_obs)*100)

srg_freq <- srgranked_df %>%
  group_by(direction) %>%
  summarise(
    high_rank_count = sum(nee_rank < end_rank, na.rm = T),
    total_obs = n(),
    .groups = "drop"
  )%>%
  mutate(perc_highrank = (high_rank_count/total_obs)*100)

srm_freq <- srmranked_df %>%
  group_by(direction) %>%
  summarise(
    high_rank_count = sum(nee_rank < end_rank, na.rm = T),
    total_obs = n(),
    .groups = "drop"
  )%>%
  mutate(perc_highrank = (high_rank_count/total_obs)*100)

wkg_freq <- wkgranked_df %>%
  group_by(direction) %>%
  summarise(
    high_rank_count = sum(nee_rank < end_rank, na.rm = T),
    total_obs = n(),
    .groups = "drop"
  )%>%
  mutate(perc_highrank = (high_rank_count/total_obs)*100 )

#----plots----------------------------
ymax <- max(
  cmw_freq$perc_highrank,
  srm_freq$perc_highrank,
  srg_freq$perc_highrank,
  wkg_freq$perc_highrank,
  na.rm = TRUE
)

# optional: round up to a nice number
ymax <- ceiling(ymax / 5) * 5

base_freq_plot <- function(df, ylab = "", ylim = c(0, ymax)) {
  ggplot(df, aes(x = direction, y = perc_highrank, group = 1)) +
    geom_point() +
    geom_smooth(method = "loess", se = F, color = "lightblue", linewidth = 1) +
    scale_x_discrete(
      breaks = function(x) {
        x[(as.numeric(as.character(x)) - 20) %% 40 == 0]
      }
    ) +
    scale_y_continuous(limits = ylim) +
    labs(x = "", y = ylab) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14)
    )
}

cmw_freq_plot <- base_freq_plot(cmw_freq, "Frequency of NEE Ranking 1–6")
srm_freq_plot <- base_freq_plot(srm_freq)
srg_freq_plot <- base_freq_plot(srg_freq)
wkg_freq_plot <- base_freq_plot(wkg_freq)

#combine plots------------------------------------------------------------------
combined_freq <- (cmw_freq_plot + srm_freq_plot + srg_freq_plot + wkg_freq_plot) +
  plot_layout(ncol = 4, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
combined_freq

