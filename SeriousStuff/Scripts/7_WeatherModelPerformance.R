## ===============================
## Model diagnostics & plots
## ===============================

library(dplyr)
library(ggplot2)
library(purrr)
library(randomForest)
library(viridis)

## Load model results

cmw_res <- readRDS("./SeriousStuff/Data/RandomForestOutputs/cmwmm_rf_results.RDS")
srg_res <- readRDS("./SeriousStuff/Data/RandomForestOutputs/srgmm_rf_results.RDS")
srm_res <- readRDS("./SeriousStuff/Data/RandomForestOutputs/srmmm_rf_results.RDS")
wkg_res <- readRDS("./SeriousStuff/Data/RandomForestOutputs/wkgmm_rf_results.RDS")

## Collapse list into df
site_list <- list(
  CMW = cmw_res,
  SRG = srg_res,
  SRM = srm_res,
  WKG = wkg_res
)

# Func to create observed vs modeled GPP plot for a site
plot_obs_vs_modeled <- function(site_res, site_name) {
  # Collapse predictions into a data frame
  pred_df <- bind_rows(
    lapply(site_res, function(x) {
      x$predictions %>%
        select(gpp_obs = gpp, gpp_pred = modeled_gpp)
    }),
    .id = "year_month"
  )
  
  # plotting:
  ggplot(pred_df, aes(x = gpp_obs, y = gpp_pred)) +
    geom_point(alpha = 0.05) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
    labs(
      x = "Observed GPP",
      y = "Modeled GPP" #, title = paste("Observed vs Modeled GPP -", site_name)
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 16) #, plot.title = element_text(size = 18, face = "bold")
    )
}

# Loop through sites to make plots; store as a list
site_plots <- map2(site_list, names(site_list), plot_obs_vs_modeled)
site_plots$CMW

###=============================================================================
## Extract variable importance (RF-style importance)

# Func to calc %IncMSE (increase in error as a variable's values are randomly excluded) importance and create plot
plot_varimp_mse <- function(site_res, site_name) {
  #%IncMSE for each year–month model
  varimp <- bind_rows(
    lapply(site_res, \(x) {
      imp <- importance(x$model)
      data.frame(
        variable = rownames(imp),
        importance = imp[, "%IncMSE"]
      )
    }),
    .id = "year_month"
  )
  
  # calc avg importance across ym combos
  varimp_mean <- varimp %>%
    group_by(variable) %>%
    summarize(mean_importance = mean(importance), .groups = "drop")
  
  #plot
  ggplot(varimp_mean, aes(x = reorder(variable, mean_importance),
                          y = mean_importance,
                          fill = mean_importance)) +
    geom_col() +
    coord_flip() +
    labs(
      x = NULL,
      y = "% Increase in MSE", #title = paste("Random Forest Variable Importance (%IncMSE) -", site_name)
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 16),
     # plot.title = element_text(size = 18, face = "bold"),
      legend.position = "none"
    )
}

# Loop over sites to create plots; store in a named list
site_varimp_plots <- map2(site_list, names(site_list), plot_varimp_mse)
site_varimp_plots$CMW

