#LAI data generated in LAI_fromNAIP.R
#  combine LAI with directional GPP to estimate directional photosynthetic capacity (PC)

#code currently maps june 2017 NAIP-derived LAI and june 2017 gpp


library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(viridis)
library(patchwork)


#load 2017 lai data-------------------------------------------------------------
geospat <- readRDS("./SeriousStuff/Data/RasterStack/GeosFlux_df468_v.rds")

# lai_df <- geospat %>%
#   filter(grepl("lai", tolower(layer))) %>%
#   select(site, direction, value) %>%
#   rename(lai = value)

lai_df <- geospat %>%
    select(site, direction, lai_premonsoon)%>%
  rename(lai = lai_premonsoon)%>%
  mutate(site = ifelse(site == "wkg", "WKG", site))
  
#prep flux data ---------------------------------------------------------------- 
deg_int    <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
sites <- list(
  CMW = "./SeriousStuff/Data/AMF_US-CMW_BASE_HH_2-5.csv",
  SRM = "./SeriousStuff/Data/AMF_US-SRM_FLUXNET_FULLSET_HH_2004-2024_4-7.csv",
  SRG = "./SeriousStuff/Data/AMF_US-SRG_FLUXNET_FULLSET_HH_2008-2024_5-7.csv",
  WKG = "./SeriousStuff/Data/AMF_US-Wkg_FLUXNET_FULLSET_HH_2004-2024_4-7.csv"
)

#func to pull relevant data and get avg directional vals
process_site <- function(file, site_name, gpp_col, wind_col, skip = 0) {
  df <- read.csv(file, na.strings = "-9999", skip = skip) %>%
    mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START))) %>%
    transmute(
      yyyy     = year(TIMESTAMP_START),
      mm       = month(TIMESTAMP_START),
      HH_UTC   = hour(TIMESTAMP_START),
      gpp      = !!sym(gpp_col),
      wind_dir = !!sym(wind_col)
    ) %>%
    filter(HH_UTC >= 8 & HH_UTC <= 17,
           yyyy == 2017,
           mm %in% 5:7) %>%
    drop_na()
  
  df_avg <- df %>%
    mutate(
      direction = as.numeric(as.character(
        cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels)
      ))
    ) %>%
    group_by(yyyy, mm, direction) %>%
    summarise(
      gpp  = mean(gpp, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df_avg)
}

#pull site data with helper function
site_plot_dfs <- list(
  CMW = process_site(sites$CMW, "CMW", "GPP_PI", "WD_1_1_1", skip = 2),
  SRM = process_site(sites$SRM, "SRM", "GPP_DT_VUT_REF", "WD"),
  SRG = process_site(sites$SRG, "SRG", "GPP_DT_VUT_REF", "WD"),
  WKG = process_site(sites$WKG, "WKG", "GPP_DT_VUT_REF", "WD")
)

gpp_df <- bind_rows(
  lapply(names(site_plot_dfs), function(s) {
    site_plot_dfs[[s]] %>%
      select(direction, gpp) %>%
      mutate(site = s)
  })
)

gpp_df <- gpp_df%>%
  group_by(site, direction)%>%
  summarise(gpp = mean(gpp, na.rm = T))%>%
  ungroup()

# combine and clean dataframe---------------------------------------------------
combd_df <- merge(lai_df, gpp_df, by = c("site", "direction"))%>%
  mutate(PC = gpp / lai)


#func for circular plots -------------------------------------------------------
plot_circular <- function(df, value_col, title_text, fill_label) {
  
  vals <- df[[value_col]]
  vmin <- min(vals, na.rm = TRUE)
  vmax <- max(vals, na.rm = TRUE)
  
  ggplot(df) +
    geom_col(
      aes(x = direction, y = 8, fill = !!sym(value_col)),
      position = "dodge2",
      alpha = 0.9
    ) +
    geom_hline(aes(yintercept = 9), color = "lightgrey") +
    coord_polar() +
    scale_fill_viridis_c(
      option = "viridis",
      name = fill_label,
      limits = c(vmin, vmax),
      breaks = c(vmin, vmax),
      labels = scales::number_format(accuracy = 0.01)
    ) +
    scale_x_continuous(
      breaks = df$direction,
      labels = df$direction
    ) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(),
      legend.position = "bottom",
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = title_text)
}

# apply plotting func
plots_list <- setNames(
  lapply(unique(combd_df$site), function(s) {
    df_site <- combd_df %>% filter(site == s)
    
    list(
      gpp_plot = plot_circular(df_site, "gpp", s, "GPP"),
      lai_plot = plot_circular(df_site, "lai", s, "LAI"),
      pc_plot  = plot_circular(df_site, "PC",  s, "PC")
    )
  }),
  unique(combd_df$site)
)


#test output
# plots_list[[which(names(site_plot_dfs) == "CMW")]]$gpp_plot
# plots_list[[which(names(site_plot_dfs) == "CMW")]]$lai_plot
# plots_list[[which(names(site_plot_dfs) == "CMW")]]$pc_plot


#arrange plots

site_order <- c("CMW", "SRM", "SRG", "WKG")

site_rows <- lapply(site_order, function(s) {
  
  p_gpp <- plots_list[[s]]$gpp_plot
  p_lai <- plots_list[[s]]$lai_plot
  p_pc  <- plots_list[[s]]$pc_plot
  
  (p_gpp | p_lai | p_pc) +
    plot_annotation(
      title = s,
      theme = theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 16,
          face = "bold"
        )
      )
    )
})

final_plot <- wrap_plots(site_rows, nrow = 2, ncol = 2)

final_plot

