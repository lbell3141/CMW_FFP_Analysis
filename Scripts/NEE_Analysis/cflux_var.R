#percent variation in GPP, NEE, and RECO

# Load libraries
library(dplyr)
library(lubridate)
library(purrr)
library(plantecophys)
library(ggplot2)
library(viridis)
library(patchwork)
library(tidyr)
library(scales)



# Load data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create primary dataframe
dat_voi <- dat_file %>%
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
    nee = NEE_PI,
    reco = RECO_PI,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

# Bin wind direction into 20° groups
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))

dat_flux <- dat_voi %>%
  select(mm, dir_group, nee, gpp, reco) %>%
  filter(if_all(everything(), ~!is.na(.)))

avg_dat_flux <- dat_flux%>%
  group_by(mm, dir_group)%>%
  summarise(across(c(gpp, nee, reco), mean, na.rm = TRUE), .groups = "drop")


avg_dat_flux <- avg_dat_flux%>%
  filter(mm %in% 6:9)


monthly_cv_summary <- avg_dat_flux%>%
  group_by(mm) %>%
  summarise(
   GPP = (sd(gpp, na.rm = TRUE) / mean(gpp, na.rm = TRUE)) * 100,
    NEE = (sd(nee, na.rm = TRUE) / mean(nee, na.rm = TRUE)) * 100,
    Respiration = (sd(reco, na.rm = TRUE) / mean(reco, na.rm = TRUE)) * 100,
    .groups = "drop"
  )

# Convert to long format for plotting
cv_long <- monthly_cv_summary %>%
  pivot_longer(cols = c(GPP, NEE, Respiration),
               names_to = "variable", values_to = "cv")%>%
  mutate(variable = factor(variable, levels = c("NEE", "GPP", "Respiration")))

# Plot
ggplot(cv_long, aes(x = mm, y = abs(cv), color = variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  scale_color_viridis_d()+
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "%CV", title = "", color = "") +
  theme_classic(base_size = 14)



#===============================================================================
#======Rank order test==========================================================
#===============================================================================
avg_flux_ranks <- avg_dat_flux %>%
  group_by(mm) %>%
  mutate(
    gpp_rank = rank(gpp, ties.method = "average"),
    nee_rank = rank(-nee, ties.method = "average"),
    reco_rank = rank(reco, ties.method = "average")
  ) %>%
  ungroup()
rank_difs <- avg_flux_ranks%>%
  mutate(nee_gpp_diff = gpp_rank - nee_rank,
         nee_reco_diff = reco_rank - nee_rank)


#----------plot heat map-----------------
rank_long <- avg_flux_ranks %>%
  pivot_longer(cols = ends_with("_rank"),
               names_to = "variable",
               values_to = "rank") %>%
  mutate(variable = recode(variable,
                           gpp_rank = "GPP",
                           nee_rank = "NEE",
                           reco_rank = "R_Eco"))

rankdif_long <- rank_difs %>%
  pivot_longer(cols = ends_with("_diff"),
               names_to = "variable",
               values_to = "rank") %>%
  mutate(variable = recode(variable,
                           nee_gpp_diff = "GPP - NEE",
                           nee_reco_diff = "Reco - NEE"))

Var_names <- c("GPP", "NEE", "Ecosystem Respiration")
var_labels <- setNames(Var_names, unique(rank_long$variable))
breaks = range(rank_long$rank, na.rm = TRUE)

heatmap_raw <- ggplot(rank_long, aes(x = as.factor(mm), y = dir_group, fill = rank)) +
  geom_tile() +
  facet_wrap(~ variable, labeller = labeller(variable = var_labels)) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "Rank",
    breaks = range(rank_long$rank, na.rm = TRUE)  # Show only min and max
  ) +
  labs(x = "Month", y = "Wind Direction", title = "") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text = element_text(size = 15, angle = 0, hjust = 0.5),
    axis.title = element_text(size = 17),
    strip.text = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 17)
  )

heatmap_raw




heatmap_diff <- ggplot(rankdif_long, aes(x = as.factor(mm), y = dir_group, fill = rank)) +
  geom_tile() +
  facet_wrap(~ variable) +
  scale_fill_gradient2(
    low = "#4575B4",   # blue
    mid = "white",     # center at zero
    high = "#D73027",  # red
    midpoint = 0,
    name = "Rank Diff"
  ) +
  labs(
    x = "Month",
    y = "Wind Direction",
    title = "Directional Rank Differences of Carbon Fluxes by Month"
  ) +
  theme_minimal(base_size = 14)




spearman_results <- avg_flux_ranks %>%
  group_by(mm) %>%
  summarise(
    cor_gpp_nee = cor(gpp_rank, nee_rank, method = "spearman"),
    cor_nee_reco = cor(reco_rank, nee_rank, method = "spearman")
  )

spearman_long <- spearman_results %>%
  pivot_longer(cols = starts_with("cor_"),
               names_to = "comparison",
               values_to = "correlation") %>%
  mutate(comparison = recode(comparison,
                             cor_gpp_nee = "GPP vs NEE",
                             cor_nee_reco = "NEE vs Reco"))


spearman_results <- spearman_results %>%
  mutate(cor_nee_reco_pos = abs(cor_nee_reco))

spearman_long <- spearman_results %>%
  select(mm, cor_gpp_nee, cor_nee_reco_pos) %>%
  pivot_longer(cols = c(cor_gpp_nee, cor_nee_reco_pos),
               names_to = "comparison",
               values_to = "correlation") %>%
  mutate(
    comparison = recode(comparison,
                        cor_gpp_nee = "GPP vs NEE",
                        cor_nee_reco_pos = "NEE vs Reco (neg)")
  )




spearman_long <- spearman_results %>%
  mutate(cor_nee_reco_pos = abs(cor_nee_reco)) %>%
  select(mm, cor_gpp_nee, cor_nee_reco_pos) %>%
  pivot_longer(cols = c(cor_gpp_nee, cor_nee_reco_pos),
               names_to = "comparison",
               values_to = "correlation") %>%
  mutate(
    comparison = recode(comparison,
                        cor_gpp_nee = "GPP and -NEE",
                        cor_nee_reco_pos = "Respiration and NEE")
  )

ggplot(spearman_long, aes(x = factor(mm, levels = 1:12), y = correlation, fill = comparison)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 1) +
  scale_fill_manual(values = c(
    "GPP and -NEE" = "#A6CEE3",
    "Respiration and NEE" = "#F4A582"
  )) +
  scale_y_continuous(
    name = "Spearman Correlation",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_x_discrete(labels = month.abb) +
  labs(
    title = "",
    x = "Month",
    fill = NULL
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "black"),
    axis.text.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black")
  )
