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

#plot all sites at once: 
pred_df <- imap_dfr(site_list, function(site_res, site_name) {
  bind_rows(
    lapply(site_res, function(x) {
      x$predictions %>%
        select(gpp_obs = gpp, gpp_pred = modeled_gpp)
    })
  ) %>%
    mutate(site = site_name)
})
r2_df <- pred_df %>%
  group_by(site) %>%
  summarise(
    r2 = summary(lm(gpp_pred ~ gpp_obs))$r.squared,
    .groups = "drop"
  ) %>%
  mutate(
    r2_label = paste0("R² = ", round(r2, 2))
  )
obs_vs_modeled_plot <- ggplot(pred_df, aes(x = gpp_obs, y = gpp_pred)) +
  geom_point(alpha = 0.05) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  geom_text(
    data = r2_df,
    aes(label = r2_label),
    x = -Inf, y = Inf,
    hjust = -0.1, vjust = 1.2,
    inherit.aes = FALSE,
    size = 5
  ) +
  facet_wrap(~ site, nrow = 1) +
  labs(
    x = "Observed GPP",
    y = "Modeled GPP"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14, face = "bold")
  )

obs_vs_modeled_plot
###=============================================================================
## Extract variable importance (RF-style importance)

#vairable names
vars <- c(ppfd = "PPFD",
          HH_UTC = "Time of Day", 
          swc = "Soil Moisture",
          temp_atmos = "Air Temp",
          rel_h = "Relative Humidity",
          wind_sp = "Wind Speed")

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
#===============================================================================
#plot all at once: 
#prep frame
all_varimp <- bind_rows(
  lapply(names(site_list), function(site_name) {
    site_res <- site_list[[site_name]]
    
    # get variable importance for each year-month
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
    
    # average across year-months
    varimp_mean <- varimp %>%
      group_by(variable) %>%
      summarize(mean_importance = mean(importance), .groups = "drop") %>%
      mutate(site = site_name)
    
    varimp_mean
  })
)

#establish site order with factor site names
all_varimp <- all_varimp %>%
  mutate(site = factor(site, levels = c("CMW", "SRM", "SRG", "WKG")))
#add full variable names
all_varimp <- all_varimp %>%
  mutate(variable = recode(variable, !!!vars))
#individual scaling for plots
all_varimp <- all_varimp %>%
  group_by(site) %>%
  mutate(importance_scaled = rescale(mean_importance)) %>%
  ungroup()
#plotting:
ggplot(all_varimp, aes(x = reorder(variable, mean_importance),
                       y = mean_importance,
                       fill = importance_scaled)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~site, ncol = 4, nrow = 1, scales = "free_y") +
  scale_fill_gradient(low = "navy", high = "lightblue") +
  labs(
    x = NULL,
    y = "% Increase in MSE",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "none",
    # remove x-axis labels for all but the first facet
    axis.text.x = element_text(size = 13),
    # strip.background = element_rect(fill = "white"),
    # custom function to blank x labels except first facet
    panel.spacing.x = unit(1.2, "lines")
  ) +
  # trick to show x-axis labels only for first facet
  theme(
    axis.text.x = element_text(),
    axis.text.x.top = element_blank()
  )

