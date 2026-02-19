library(mcr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(viridis)
library(patchwork)

# To help prep data-
deg_int    <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
sites <- list(
  CMW = "./SeriousStuff/Data/FluxData/PI_DATA/Full_PI_Data_by_Site/CMW_FullFluxes.csv",
  SRM = "./SeriousStuff/Data/FluxData/PI_DATA/Full_PI_Data_by_Site/SRM_FullFluxes.csv",
  SRG = "./SeriousStuff/Data/FluxData/PI_DATA/Full_PI_Data_by_Site/SRG_FullFluxes.csv",
  WKG = "./SeriousStuff/Data/FluxData/PI_DATA/Full_PI_Data_by_Site/WKG_FullFluxes.csv"
)

# Helper function to process each site
process_site <- function(file, site_name, nee_col, gpp_col, reco_col, wind_col, skip = 0) {
  df <- read.csv(file, na.strings = "-9999", skip = skip) %>%
    mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START))) %>%
    transmute(
      yyyy     = year(TIMESTAMP_START),
      mm       = month(TIMESTAMP_START),
      HH_UTC   = hour(TIMESTAMP_START),
      nee      = as.numeric(!!sym(nee_col)),
      gpp      = as.numeric(!!sym(gpp_col)),
      reco     = as.numeric(!!sym(reco_col)),
      wind_dir = as.numeric(!!sym(wind_col))
    )%>%
    filter(HH_UTC >= 8 & HH_UTC <= 17) %>%
    drop_na()
  
  df_avg <- df %>%
    mutate(
      direction = as.numeric(as.character(
        cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels)
      ))
    ) %>%
    group_by(yyyy, mm, direction) %>%
    summarise(
      nee  = mean(nee, na.rm = TRUE),
      gpp  = mean(gpp, na.rm = TRUE),
      reco = mean(reco, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_ranked <- df_avg %>%
    group_by(yyyy, mm) %>%
    mutate(
      nee_rank     = rank(nee, ties.method = "average"),
      neg_nee_rank = rank(-nee, ties.method = "average"),
      gpp_rank     = rank(gpp, ties.method = "average"),
      reco_rank    = rank(reco, ties.method = "average")
    ) %>%
    ungroup() %>%
    mutate(site = site_name)
  
  # Prepare plot_df with two groups
  plot_df <- bind_rows(
    df_ranked %>% transmute(site, x = neg_nee_rank, y = gpp_rank, group = "-NEE vs GPP"),
    df_ranked %>% transmute(site, x = nee_rank, y = reco_rank, group = "NEE vs RECO")
  )
  
  return(plot_df)
}

#pull site data with helper function
site_plot_dfs <- list(
  CMW = process_site(sites$CMW, "CMW", "NEE_PI", "GPP_PI", "RECO_PI", "WD_1_1_1", skip = 2),
  SRM = process_site(sites$SRM, "SRM", "NEE", "GPP", "RECO", "WD_1_1_1"),
  SRG = process_site(sites$SRG, "SRG", "NEE", "GPP", "RECO", "WD_1_1_1"),
  WKG = process_site(sites$WKG, "WKG", "NEE", "GPP", "RECO", "WD_1_1_1")
)

all_plot_df <- bind_rows(site_plot_dfs)

# site order
all_plot_df$site <- factor(all_plot_df$site, levels = c("CMW","SRM","SRG","WKG"))

#add R2 from a lm
r2_df <- all_plot_df %>%
  group_by(site, group) %>%
  summarise(
    r2 = {
      fit <- mcreg(x, y, method.reg = "Deming")
      cor(x, y)^2
    },
    .groups = "drop"
  ) %>%
  mutate(
    r2_label = paste0("R² = ", round(r2, 3))
  )


make_density_plot <- function(df, group_name, y_label) {
  df_sub <- df %>% filter(group == group_name)
  r2_sub <- r2_df %>% filter(group == group_name)
  
  x_label <- ifelse(group_name == "-NEE vs GPP", "Ranked -NEE", "Ranked NEE")
  
  ggplot(df_sub, aes(x = x, y = y)) +
    geom_density_2d_filled(aes(fill = after_stat(level)), alpha = 0.6) +
    stat_smooth(method = "lm", se = FALSE, color = NA)+
    geom_abline(
      data = df_sub %>%
        group_by(site) %>%
        summarise(
          slope = {
            fit <- mcreg(x, y, method.reg = "Deming")
            fit@para[2]
          },
          intercept = {
            fit <- mcreg(x, y, method.reg = "Deming")
            fit@para[1]
          }
        ),
      aes(slope = slope, intercept = intercept),
      color = "red",
      linetype = "dashed",
      inherit.aes = FALSE
    )+
  geom_text(
      data = r2_sub,
      aes(label = r2_label),
      x = -Inf, y = Inf,
      hjust = -0.3, vjust = 2,
      inherit.aes = FALSE,
      size = 5,
      color = "white"
    ) +
    scale_x_continuous(breaks = c(1, 18)) +
    scale_y_continuous(breaks = c(1, 18)) +
    coord_cartesian(xlim = c(1, 18), ylim = c(1, 18), expand = FALSE)+
  scale_fill_viridis_d(
      name = "Point density",
      guide = guide_legend(
        label.position = "right",
        title.position = "top"
      )
    ) +
    labs(
      title = NULL,
      x = x_label,
      y = y_label
    ) +
    facet_wrap(~ site, nrow = 1) +
    theme_minimal(base_size = 12) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text = element_text(size = 12),
      axis.ticks = element_line(),
      panel.grid = element_blank(),
      panel.spacing = unit(1.2, "lines")
    )
}

#plot
row1 <- make_density_plot(all_plot_df, "-NEE vs GPP", "Ranked GPP")
row2 <- make_density_plot(all_plot_df, "NEE vs RECO", "Ranked RECO")

combined <- row1 / row2 + plot_layout(guides = "collect") & theme(legend.position = "right")
combined

