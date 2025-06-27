#model heat map and scatter for sup figs

predict_dir_group_df <- function(df) {
  rf_model <- randomForest(gpp ~ ., data = df)
  df %>%
    mutate(
      modeled_gpp = predict(rf_model, df)
    ) %>%
    select(dir_group, observed = gpp, predicted = modeled_gpp)
}
predictions_list <- lapply(mm_split_dat, predict_dir_group_df)
all_predictions <- bind_rows(predictions_list, .id = "dataset_id")
