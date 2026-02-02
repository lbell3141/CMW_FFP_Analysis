# make a table of statistical relationships between GPP/carbon fluxes and directional 
# geospatial values (estimated in CreateGeoSpatFrames.R)

library(dplyr)
library(tidyr)
library(lubridate)
library(gt)
library(stringr)


geospat <- readRDS("./SeriousStuff/Data/RasterStack/DirGeosDf357.rds")

# pull summer fluxes for sites--------------------------------------------------
deg_int    <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
sites <- list(
  CMW = "./SeriousStuff/Data/AMF_US-CMW_BASE_HH_2-5.csv",
  SRM = "./SeriousStuff/Data/AMF_US-SRM_FLUXNET_FULLSET_HH_2004-2024_4-7.csv",
  SRG = "./SeriousStuff/Data/AMF_US-SRG_FLUXNET_FULLSET_HH_2008-2024_5-7.csv",
  Wkg = "./SeriousStuff/Data/AMF_US-Wkg_FLUXNET_FULLSET_HH_2004-2024_4-7.csv"
)

process_site <- function(file, site_name, nee_col, gpp_col, reco_col, wind_col, skip = 0) {
  df <- read.csv(file, na.strings = "-9999", skip = skip) %>%
    mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START))) %>%
    transmute(
      yyyy     = year(TIMESTAMP_START),
      mm       = month(TIMESTAMP_START),
      HH_UTC   = hour(TIMESTAMP_START),
      nee      = !!sym(nee_col),
      gpp      = !!sym(gpp_col),
      reco     = !!sym(reco_col),
      wind_dir = !!sym(wind_col)
    ) %>%
    filter(HH_UTC >= 8 & HH_UTC <= 17,
           mm %in% 5:7) %>%
    drop_na()
  
  df_avg <- df %>%
    mutate(
      direction = as.numeric(as.character(
        cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels)
      )),
      site = site_name
    ) %>%
    group_by(site, direction) %>%
    summarise(
      nee  = mean(nee, na.rm = TRUE),
      gpp  = mean(gpp, na.rm = TRUE),
      reco = mean(reco, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df_avg)
}

#pull site data with function
site_plot_dfs <- list(
  CMW = process_site(sites$CMW, "CMW", "NEE_PI", "GPP_PI", "RECO_PI", "WD_1_1_1", skip = 2),
  SRM = process_site(sites$SRM, "SRM", "NEE_VUT_REF", "GPP_DT_VUT_REF", "RECO_DT_VUT_REF", "WD"),
  SRG = process_site(sites$SRG, "SRG", "NEE_VUT_REF", "GPP_DT_VUT_REF", "RECO_DT_VUT_REF", "WD"),
  Wkg = process_site(sites$Wkg, "Wkg", "NEE_VUT_REF", "GPP_DT_VUT_REF", "RECO_DT_VUT_REF", "WD")
)

all_plot_df <- bind_rows(site_plot_dfs)%>%
  drop_na()

#combine geospat and tower data-------------------------------------------------

combined_df <- all_plot_df %>%
  merge(geospat, by = c("site", "direction"))

combined_wide <- combined_df %>%
  pivot_wider(
    id_cols = c(site, direction, nee, gpp, reco),
    names_from = layer,
    values_from = value
  )
combined_wide_agg <- combined_wide %>%
  rowwise() %>%
  mutate(
    chm = mean(c_across(starts_with("chm_dir")), na.rm = TRUE),
    twi = mean(c_across(starts_with("twi_dir")), na.rm = TRUE),
    lai = mean(c_across(starts_with("lai_dir")), na.rm = TRUE),
    cancov = mean(c_across(starts_with("cancov_dir")), na.rm = TRUE),
    ndvi = mean(c_across(starts_with("ndvi_dir")), na.rm = TRUE)
  ) %>%
  ungroup()%>%
  select(site, direction, nee, gpp, reco, cancov, chm, ndvi, lai, twi)

#saveRDS(combined_wide_agg, "./SeriousStuff/Data/RasterStack/GeosFlux_df468.rds")  
  
# pull geos vars
geo_vars <- names(combined_wide_agg)[!names(combined_wide_agg) %in% c("site", "direction", "gpp", "nee", "reco")]

#func to calc stats at a given site: pearson's r and regression values
compute_site_stats <- function(df_site, flux = "gpp", geo_vars) {
  
  map_dfr(geo_vars, function(var) {
    
    x <- df_site[[var]]
    y <- df_site[[flux]]
    
    # remove NA pairs
    ok <- complete.cases(x, y)
    x  <- x[ok]
    y  <- y[ok]
    
    if (length(x) < 3) {
      return(data.frame(
        variable = var,
        cor = NA,
        slope = NA,
        r2 = NA,
        p = NA,
        sig = ""
      ))
    }
    
    lm_fit <- lm(y ~ x)
    summ   <- summary(lm_fit)
    
    p_val <- summ$coefficients[2, "Pr(>|t|)"]
    
    sig <- case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      TRUE          ~ ""
    )
    
    data.frame(
      variable = var,
      cor      = cor(x, y),
      slope    = coef(lm_fit)[2],
      r2       = summ$r.squared,
      p        = p_val,
      sig      = sig
    )
  })
}

stat_table <- combined_wide_agg %>%
  group_by(site) %>%
  group_modify(~ compute_site_stats(.x, flux = "gpp", geo_vars)) %>%
  ungroup()

#write.csv(stat_table, "./SeriousStuff/Data/RasterStack/GeosFluxStats.csv")

# 
# stat_table %>%
#   mutate(
#     cor   = round(cor, 2),
#     slope = round(slope, 3),
#     r2    = round(r2, 2),
#     p     = signif(p, 2)
#   ) %>%
#   arrange(site, variable) %>%
#   gt(groupname_col = "site") %>%
#   tab_header(
#     title = "Relationships between GPP and Geospatial Variables",
#     subtitle = "Directional footprint–weighted predictors"
#   ) %>%
#   cols_label(
#     variable = "Geospatial variable",
#     cor      = "Pearson r",
#     slope    = "Slope",
#     r2       = expression(R^2),
#     p        = "p-value",
#     sig      = ""
#   ) %>%
#   tab_source_note(
#     source_note = "Significance codes: * p < 0.05, ** p < 0.01, *** p < 0.001"
#   )

stat_table <- stat_table %>%
  mutate(sig = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  ))

stat_table <- stat_table %>%
  mutate(
    cor_val   = ifelse(!is.na(cor), paste0(round(cor, 2), sig), NA),
    slope_val = ifelse(!is.na(slope), paste0(round(slope, 2), sig), NA),
    r2_val    = ifelse(!is.na(r2), paste0(round(r2, 2), sig), NA),
    p_val     = ifelse(!is.na(p), paste0(round(p, 2), sig), NA)
  )

stat_table_wide <- stat_table %>%
  select(site, variable, cor_val, slope_val, r2_val, p_val) %>%
  pivot_longer(
    cols = c(cor_val, slope_val, r2_val, p_val),
    names_to = "stat",
    values_to = "value"
  ) %>%
  unite("site_stat", site, stat) %>%
  pivot_wider(
    names_from = site_stat,
    values_from = value
  )

stat_table_wide <- stat_table_wide %>%
  mutate(variable = recode(variable,
                           cancov = "% Canopy Cover",
                           chm    = "Canopy Height",
                           ndvi   = "NDVI",
                           lai    = "LAI",
                           twi    = "TWI"
  ))

colnames(stat_table_wide) <- colnames(stat_table_wide) %>%
  str_replace("_cor_val$", " Correlation") %>%
  str_replace("_slope_val$", " Slope") %>%
  str_replace("_r2_val$", " R²") %>%
  str_replace("_p_val$", " p-value")

gt_tbl <- stat_table_wide %>%
  gt(rowname_col = "variable") %>%
  tab_header(
    title = "GPP and Geospatial Data Relationships",
    subtitle = " * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_spanner(label = "CMW", columns = contains("CMW")) %>%
  tab_spanner(label = "SRM", columns = contains("SRM")) %>%
  tab_spanner(label = "SRG", columns = contains("SRG")) %>%
  tab_spanner(label = "Wkg", columns = contains("Wkg")) %>%
  tab_options(
    table.font.size = 12,
    data_row.padding = px(8)
  )