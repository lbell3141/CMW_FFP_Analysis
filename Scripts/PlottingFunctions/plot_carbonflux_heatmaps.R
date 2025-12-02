#plot heatmaps of GPP, NEE, and Reco

plot_carbonflux_heatmaps <- function(avgdat){
  
avg_flux_ranks <- avgdat %>%
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
                           nee_rank = "-NEE",
                           reco_rank = "R_Eco"))

rankdif_long <- rank_difs %>%
  pivot_longer(cols = ends_with("_diff"),
               names_to = "variable",
               values_to = "rank") %>%
  mutate(variable = recode(variable,
                           nee_gpp_diff = "GPP - NEE",
                           nee_reco_diff = "Reco - NEE"))
rank_long$variable <- factor(rank_long$variable, levels = c("GPP", "-NEE", "R_Eco"))

Var_names <- c("GPP", "-NEE", "Ecosystem Respiration")
var_labels <- setNames(Var_names, unique(rank_long$variable))

heatmap_raw <- ggplot(rank_long, aes(x = as.factor(mm), y = direction, fill = rank)) +
  geom_tile() +
  facet_wrap(~ variable, labeller = labeller(variable = var_labels)) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "Rank",
    breaks = range(rank_long$rank, na.rm = TRUE)
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

return(heatmap_raw)
}
