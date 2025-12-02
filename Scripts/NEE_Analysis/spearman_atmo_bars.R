# Rank variables within each month across directions
rank_long <- dir_dat_avg %>%
  pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value") %>%
  group_by(mm, variable) %>%
  mutate(rank = rank(value, ties.method = "average")) %>%
  ungroup()
# Get rank of GPP
gpp_rank <- rank_long %>%
  filter(variable == "gpp") %>%
  select(mm, direction, gpp_rank = rank)

# Join with other variables
rank_corr <- rank_long %>%
  filter(variable != "gpp") %>%
  left_join(gpp_rank, by = c("mm", "direction")) %>%
  group_by(mm, variable) %>%
  summarise(spearman_rho = cor(rank, gpp_rank, method = "spearman"), .groups = "drop")
# Month labels
rank_corr$month <- factor(month.abb[rank_corr$mm], levels = month.abb[months_to_plot])

# Plot
ggplot(rank_corr, aes(x = month, y = spearman_rho, group = variable, color = variable)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  labs(
    x = "Month",
    y = "Spearman's Rank Correlation (ρ)",
    color = "Variable",
    title = "Correlation of GPP Rank with Atmospheric Drivers by Month"
  ) +
  theme_minimal(base_size = 14)








# Define variable labels
var_labels <- c(
  ppfd = "PPFD",
  rel_h = "RH",
  swc = "SWC",
  temp_atmos = "Air Temp",
  wind_sp = "Wind Sp"
)

# Prepare the data
rank_corr_sig_bars <- rank_corr_sig %>%
  mutate(
    variable = recode(variable, !!!var_labels),
    variable = factor(variable, levels = var_labels),  # control order
    rho_abs = abs(spearman_rho),
    month = factor(month.abb[mm], levels = month.abb[months_to_plot])
  )
ggplot(rank_corr_sig_bars, aes(x = variable, y = rho_abs, fill = sign)) +
  geom_col(width = 0.7) +
  
  # Spearman's rho value label (higher above the bar)
  geom_text(
    aes(label = round(spearman_rho, 2)),
    position = position_nudge(y = 0.05),  # move label up
    size = 4
  ) +
  
  # Significance stars (just above bar)
  geom_text(
    aes(label = significance),
    position = position_nudge(y = 0.01),
    size = 6
  ) +
  
  scale_fill_manual(
    values = c("Positive" = "#1b9e77", "Negative" = "#d95f02")
  ) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  facet_wrap(~ month, nrow = 1) +
  labs(
    x = NULL,
    y = "Spearman's Correlation Coefficient",
    fill = "Correlation Sign"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

