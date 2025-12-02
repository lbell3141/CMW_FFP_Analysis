rank_long <- avg_flux_ranks %>%
  pivot_longer(cols = ends_with("_rank"),
               names_to = "variable",
               values_to = "rank") %>%
  mutate(variable = recode(variable,
                           gpp_rank = "GPP",
                           nee_rank = "-NEE",
                           reco_rank = "R_Eco"),
         # flip the ranking for Reco so higher loss = higher rank
         rank = ifelse(variable == "R_Eco", max(rank, na.rm = TRUE) - rank + 1, rank))

rank_long$variable <- factor(rank_long$variable, levels = c("GPP", "-NEE", "R_Eco"))

Var_names <- c("GPP", "-NEE", "-Ecosystem Respiration")
var_labels <- setNames(Var_names, levels(rank_long$variable))

heatmap_raw <- ggplot(rank_long, aes(x = as.factor(mm), y = dir_group, fill = rank)) +
  geom_tile() +
  facet_wrap(~ variable, labeller = labeller(variable = var_labels)) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "Rank",
    breaks = pretty(rank_long$rank, n = 5)
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
