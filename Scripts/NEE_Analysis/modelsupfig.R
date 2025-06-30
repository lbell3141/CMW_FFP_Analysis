#model heat map and scatter for sup figs

predict_dir_group_df <- function(df) {
  rf_model <- randomForest(gpp ~ ., data = df)
  df %>%
    mutate(
      modeled_gpp = predict(rf_model, df)
    ) %>%
    select(dir_group, observed = gpp, predicted = modeled_gpp)
}

#predictions_list <- lapply(mm_split_dat, predict_dir_group_df)
#saveRDS(predictions_list, "./Data/30minpredictionsRF.RDS")
all_predictions <- bind_rows(predictions_list, .id = "dataset_id")

mod_scatter <- ggplot(all_predictions, aes(x = observed, y = predicted))+
  geom_point()+
  theme_classic()





mod_scatter <- ggplot(all_predictions, aes(x = predicted, y = observed)) +
  geom_point(color = "black", alpha = 0.2, size = 0.4) +
  # stat_density_2d(aes(color = after_stat(level)),
  #                 geom = "density_2d", 
  #                 contour = TRUE,
  #                 size = 0.5,
  #                 color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
  labs(x = expression("Modeled GPP"),
       y = expression("Measured GPP")) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 15)
  )

#===============================================================================
#======Rank order test==========================================================
#===============================================================================

mm_rf <- readRDS("./Data/mm_rf_results.RDS")
mm_mod_list <- lapply(mm_rf, \(x) x$summary)
mm_avg_gpp <- bind_rows(mm_mod_list, .id = "Month")

avg_mod_ranks <- mm_avg_gpp %>%
  group_by(Month) %>%
  mutate(
    obs_rank = rank(real_avg_gpp, ties.method = "average"),
    pred_rank = rank(mod_avg_gpp, ties.method = "average"),
    dif_rank = obs_rank - pred_rank
  ) %>%
  ungroup()

#----------plot heat map-----------------
mod_rank_long <- avg_mod_ranks %>%
  pivot_longer(cols = ends_with("_rank"),
               names_to = "variable",
               values_to = "rank")

Var_names <- c("Observed", "Modeled")
var_labels <- setNames(Var_names, unique(mod_rank_long$variable))
breaks = range(mod_rank_long$rank, na.rm = TRUE)

mod_rank_long$Month <- factor(mod_rank_long$Month, levels = 1:12)

heatmap_mod <- ggplot(mod_rank_long, aes(x = Month, y = dir_group, fill = rank)) +
  geom_tile() +
  facet_wrap(~ variable, labeller = labeller(variable = var_labels)) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "Rank",
    breaks = range(mod_rank_long$rank, na.rm = TRUE)
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

avg_mod_ranks$Month <- factor(avg_mod_ranks$Month, levels = 1:12)

rank_dif <- ggplot(avg_mod_ranks, aes(x = Month, y = dir_group, fill = dif_rank)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4575B4", mid = "white", high = "#D73027", midpoint = 0) +
  theme_minimal() +
  labs(
    x = "Month",
    y = "Wind Direction",
    title = "Directional Rank Residuals",
    fill = "Rank\nDifference"
  ) +
  theme(plot.title = element_text(size = 19), 
    axis.text = element_text(size = 15, angle = 0, hjust = 0.5),
    axis.title = element_text(size = 17),
    strip.text = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 17)
  )


mod_scatter + rank_dif



#------------------------------------
#----------calc stats-----------------
#----------------------------------
#
#

spearman_results <- avg_mod_ranks %>%
  mutate(Month = factor(Month, levels = 1:12))%>%
  group_by(Month) %>%
  summarise(
    cor_mod_obs = cor(pred_rank, obs_rank, method = "spearman"),
  )
