library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(lubridate)
library(purrr)
library(patchwork)

# load RF data (from WeatherModelOutputs.R)
site_files <- list(
  CMW = "./SeriousStuff/Data/RandomForestOutputs/cmwmm_rf_summary_results.RDS",
  SRG = "./SeriousStuff/Data/RandomForestOutputs/srgmm_rf_summary_results.RDS",
  SRM = "./SeriousStuff/Data/RandomForestOutputs/srmmm_rf_summary_results.RDS",
  WKG = "./SeriousStuff/Data/RandomForestOutputs/wkgmm_rf_summary_results.RDS"
)

site_summaries <- imap(site_files, ~{
  readRDS(.x) %>%
    bind_rows(.id = "ym_id") %>%
    mutate(site = .y)
})

all_sites_df <- bind_rows(site_summaries)

# Prep plotting dfs/components 
#distinct years for plot
fingerprint_df <- all_sites_df %>%
  rename(ym = ym_id) %>%
  separate(ym, into = c("yyyy", "mm"), sep = "\\.", convert = TRUE, remove = FALSE)

year_breaks <- fingerprint_df %>%
  group_by(site, yyyy) %>%
  summarise(
    first_ym = first(ym),
    .groups = "drop"
  )%>%
  mutate(first_ym_num = as.numeric(first_ym))

year_labels <- fingerprint_df %>%
  distinct(site, yyyy, ym) %>%
  group_by(site, yyyy) %>%
  summarise(
    ym_year = first(ym),
    .groups = "drop"
  )

# over/under estimate coding
fingerprint_df <- fingerprint_df %>%
  mutate(
    bias = case_when(
      diff_avg_gpp > 0  ~ "Overestimate",
      diff_avg_gpp < 0  ~ "Underestimate",
      T       ~ "Neutral"
    )
  )

#plot

get_bias_limits <- function(df) {
  quantile(df$diff_avg_gpp, probs = c(0.025, 0.975), na.rm = TRUE)
}
plot_site_residuals <- function(df, site_name, bias_limits, interval = 2) {
  
  site_df <- df %>% filter(site == site_name)
  
  # Ensure numeric columns for ordering
  site_df <- site_df %>%
    mutate(
      yyyy = as.numeric(yyyy),
      mm = as.numeric(mm),
      direction = factor(direction, levels = sort(unique(df$direction)))
    ) %>%
    arrange(yyyy, mm) %>%
    mutate(
      ym = factor(paste0(yyyy, ".", mm), levels = unique(paste0(yyyy, ".", mm)))
    )
  
  # One ym per year for y-axis labels
  ym_labels <- site_df %>%
    group_by(yyyy) %>%
    slice(1) %>%
    pull(ym)
  
  # Select x-axis breaks using interval
  x_breaks <- levels(site_df$direction)[seq(1, length(levels(site_df$direction)), by = interval)]
  
  ggplot(site_df, aes(x = direction, y = ym, fill = diff_avg_gpp)) +
    geom_tile(color = NA, alpha = 1) +
    scale_fill_gradient2(
      low = "navy",
      mid = "white",
      high = "darkred",
      midpoint = 0,
      limits = bias_limits,
      breaks = bias_limits,                  # only show min and max
      labels = round(bias_limits, 2),
      name = "Model residual"
    ) +
    scale_y_discrete(
      breaks = ym_labels,
      labels = substr(ym_labels, 1, 4)
    ) +
    scale_x_discrete(
      breaks = x_breaks
    ) +
    labs(
      x = "Wind Direction (°)",
      y = "Year",
      title = site_name
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5
      )
    )
}
# Global 95% bias limits
cmw_bias_limits <- quantile(fingerprint_df %>% filter(site == "CMW") %>% pull(diff_avg_gpp),
                            probs = c(0.025, 0.975), na.rm = TRUE)
srm_bias_limits <- quantile(fingerprint_df %>% filter(site == "SRM") %>% pull(diff_avg_gpp),
                            probs = c(0.025, 0.975), na.rm = TRUE)
srg_bias_limits <- quantile(fingerprint_df %>% filter(site == "SRG") %>% pull(diff_avg_gpp),
                            probs = c(0.025, 0.975), na.rm = TRUE)
wkg_bias_limits <- quantile(fingerprint_df %>% filter(site == "WKG") %>% pull(diff_avg_gpp),
                            probs = c(0.025, 0.975), na.rm = TRUE)

# Plot each site — each will have its own legend
cmw <- plot_site_residuals(fingerprint_df, "CMW", cmw_bias_limits, interval = 2)
srm <- plot_site_residuals(fingerprint_df, "SRM", srm_bias_limits, interval = 2)
srg <- plot_site_residuals(fingerprint_df, "SRG", srg_bias_limits, interval = 2)
wkg <- plot_site_residuals(fingerprint_df, "WKG", wkg_bias_limits, interval = 2)

# Combine
combined_sites <- cmw + srm + srg + wkg +
  plot_layout(ncol = 4)  # no 'guides = "collect"', so each keeps its own legend

combined_sites



#===============================================================================
# Function to summarize over/underestimates
# Function to summarize underestimates only
freq_underestimate <- function(df) {
  df %>%
    group_by(direction) %>%
    summarise(
      under_count = sum(bias == "Underestimate", na.rm = TRUE),
      .groups = "drop"
    )%>%
    mutate(direction = factor(direction, levels = sort(unique(direction))))
}
cmw_under <- freq_underestimate(fingerprint_df %>% filter(site == "CMW"))
srg_under <- freq_underestimate(fingerprint_df %>% filter(site == "SRG"))
srm_under <- freq_underestimate(fingerprint_df %>% filter(site == "SRM"))
wkg_under <- freq_underestimate(fingerprint_df %>% filter(site == "WKG"))

base_under_plot <- function(df, ylab = "") {
  ggplot(df, aes(x = direction, y = under_count, group = 1)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "darkblue", linewidth = 1) +
    scale_x_discrete(
      breaks = function(x) {
        x[(as.numeric(as.character(x)) - 20) %% 40 == 0]
      }
    ) +
    labs(x = "", y = ylab) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(angle = 45, hjust = 1, size = 12)
    )
}
cmw_plot <- base_under_plot(cmw_under, "Frequency of Underestimated GPP")
srm_plot <- base_under_plot(srm_under)
srg_plot <- base_under_plot(srg_under)
wkg_plot <- base_under_plot(wkg_under)

combined_under <- (cmw_plot + srm_plot + srg_plot + wkg_plot) +
  plot_layout(ncol = 4)

combined_under


#===============================================================================
#Fingerprints with z-score residuals (y-m zscore base)

fingerprint_df <- fingerprint_df %>%
  group_by(site, mm) %>%
  mutate(
    diff_avg_gpp_z = (diff_avg_gpp - mean(diff_avg_gpp, na.rm = TRUE)) /
      sd(diff_avg_gpp, na.rm = TRUE)
  ) %>%
  ungroup()
z_limits <- c(-2.5, 2.5)
plot_site_residuals <- function(df, site_name, z_limits, interval = 2) {
  
  site_df <- df %>% filter(site == site_name)
  
  site_df <- site_df %>%
    mutate(
      yyyy = as.numeric(yyyy),
      mm = as.numeric(mm),
      direction = factor(direction, levels = sort(unique(df$direction)))
    ) %>%
    arrange(yyyy, mm) %>%
    mutate(
      ym = factor(paste0(yyyy, ".", mm), levels = unique(paste0(yyyy, ".", mm)))
    )
  
  ym_labels <- site_df %>%
    group_by(yyyy) %>%
    slice(1) %>%
    pull(ym)
  
  x_breaks <- levels(site_df$direction)[seq(1, length(levels(site_df$direction)), by = interval)]
  
  ggplot(site_df, aes(x = direction, y = ym, fill = diff_avg_gpp_z)) +
    geom_tile(color = NA) +
    scale_fill_gradient2(
      low = "navy",
      mid = "white",
      high = "darkred",
      midpoint = 0,
      limits = z_limits,
      breaks = z_limits,
      labels = z_limits,
      name = "Residual z-score"
    ) +
    scale_y_discrete(
      breaks = ym_labels,
      labels = substr(ym_labels, 1, 4)
    ) +
    scale_x_discrete(breaks = x_breaks) +
    labs(
      x = "Wind Direction (°)",
      y = "Year",
      title = site_name
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5
      )
    )
}
z_limits <- c(-2, 2)

cmw <- plot_site_residuals(fingerprint_df, "CMW", z_limits, interval = 2)
srm <- plot_site_residuals(fingerprint_df, "SRM", z_limits, interval = 2)
srg <- plot_site_residuals(fingerprint_df, "SRG", z_limits, interval = 2)
wkg <- plot_site_residuals(fingerprint_df, "WKG", z_limits, interval = 2)

combined_sites <- cmw + srm + srg + wkg +
  plot_layout(ncol = 4)
combined_sites



#===============================================================================

#===============================================================================

#===============================================================================
# Function to summarize over/underestimates
# Function to summarize underestimates only
freq_under_percent <- function(df) {
  df %>%
    group_by(direction) %>%
    summarise(
      n_total = n(),
      n_under = sum(bias == "Underestimate", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      percent_under = 100 * n_under / n_total,
      direction = factor(direction, levels = sort(unique(direction)))
    )
}

global_ymax <- max(
  cmw_under$percent_under,
  srm_under$percent_under,
  srg_under$percent_under,
  wkg_under$percent_under,
  na.rm = TRUE
)

global_ymax <- ceiling(global_ymax / 5) * 5


cmw_under <- freq_under_percent(fingerprint_df %>% filter(site == "CMW"))
srg_under <- freq_under_percent(fingerprint_df %>% filter(site == "SRG"))
srm_under <- freq_under_percent(fingerprint_df %>% filter(site == "SRM"))
wkg_under <- freq_under_percent(fingerprint_df %>% filter(site == "WKG"))


base_under_plot <- function(df, ylab = "") {
  ggplot(df, aes(x = direction, y = percent_under, group = 1)) +
    geom_point(size = 2) +
    geom_smooth(method = "loess", se = FALSE,
                color = "darkblue", linewidth = 1) +
    scale_x_discrete(
      breaks = function(x) {
        x[(as.numeric(as.character(x)) - 20) %% 40 == 0]
      }
    ) +
    scale_y_continuous(
      limits = c(0, global_ymax),
      breaks = c(0, 30, 60,global_ymax),
      labels = scales::percent_format(scale = 1)
    ) +
    labs(x = "", y = ylab) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12)
    )
}

cmw_plot <- base_under_plot(cmw_under, "Percent underestimated GPP")
srm_plot <- base_under_plot(srm_under)
srg_plot <- base_under_plot(srg_under)
wkg_plot <- base_under_plot(wkg_under)

combined_under <- (cmw_plot + srm_plot + srg_plot + wkg_plot) +
  plot_layout(ncol = 4)
combined_under
