#run dfs generated in WeatherModelOutputs.R

library(dplyr)
library(purrr)
library(ggplot2)
library(viridis)

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

ranked_df <- all_sites_df %>%
  group_by(site, ym_id) %>%
  mutate(
    gpp_rank   = rank(obs_avg_gpp, ties.method = "average", na.last = "keep"),
    resid_rank = rank(diff_avg_gpp, ties.method = "average", na.last = "keep")
  ) %>%
  ungroup()


plot_df <- ranked_df %>%
  transmute(
    site,
    ym_id,
    direction,
    x = gpp_rank,
    y = resid_rank
  )

plot_df$site <- factor(plot_df$site, levels = c("CMW", "SRM", "SRG", "WKG"))

# plot
ggplot(plot_df, aes(x = x, y = y)) +
  geom_density_2d_filled(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ site, nrow = 1) +  # force one row
  labs(
    x = "Ranked Directional GPP",
    y = "Ranked Directional GPP Residual"
  ) +
  theme_minimal()
