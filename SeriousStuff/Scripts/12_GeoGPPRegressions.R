library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(stringr)

#===============================================================================
# 1. load 30min RF predictions
#===============================================================================

site_files <- list(
  CMW = "./SeriousStuff/Data/RandomForestOutputs/cmwmm_rf_resultsPI.RDS",
  SRM = "./SeriousStuff/Data/RandomForestOutputs/srmmm_rf_resultsPI.RDS",
  SRG = "./SeriousStuff/Data/RandomForestOutputs/srgmm_rf_resultsPI.RDS",
  WKG = "./SeriousStuff/Data/RandomForestOutputs/wkgmm_rf_resultsPI.RDS"
)

site_summaries <- imap(site_files, ~{
  readRDS(.x) %>%
    map_dfr(~ .x$predictions, .id = "ym_id") %>%
    mutate(site = .y)
})

all_df <- site_summaries %>%
  bind_rows() %>%
  mutate(
    date = make_datetime(yyyy, mm, day, HH_UTC, MM)
  )

#===============================================================================
# 2. Prep season definitions
#===============================================================================

all_df <- all_df %>%
  mutate(
    season = case_when(
      mm %in% c(4,5,6)  ~ "Premonsoon",
      mm %in% c(7,8,9)  ~ "Monsoon",
      mm %in% c(10,11)  ~ "Postmonsoon",
      TRUE              ~ "Winter"
    )
  )

#===============================================================================
# 3. calc mean seasonal residual flux and add seasons
#===============================================================================

flux_seasonal <- all_df %>%
  group_by(site, yyyy, season, dir_group) %>%
  summarise(
    residual = mean(resid_gpp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(direction = dir_group) %>%
  mutate(direction = as.numeric(as.character(direction)))

flux_seasonal <- flux_seasonal %>%
  mutate(
    season = factor(season,
                    levels = c("Winter","Premonsoon","Monsoon","Postmonsoon"))
  )

#===============================================================================
# 4. load spatial data
#===============================================================================

spatial_df <- read.csv("./SeriousStuff/Data/RasterStack/MaskedSpatialData.csv")

#===============================================================================
# 5. pull static variables (height, cover, topo)
#===============================================================================

spatial_static <- spatial_df %>%
  filter(variable %in% c("twi", "chm", "cancov")) %>%
  rename(value = weighted_mean) %>%
  mutate(
    predictor = case_when(
      variable == "twi" ~ "Topography",
      variable == "chm" ~ "Canopy Height",
      variable == "cancov" ~ "Canopy Cover"
    )
  ) %>%
  select(site, direction, predictor, value)

#===============================================================================
# 6. format dynamic variables (lai, ndvi)
#===============================================================================

spatial_seasonal <- spatial_df %>%
  filter(variable %in% c("ndvi", "lai")) %>%
  mutate(
    variable = tolower(variable),
    
    season = case_when(
      mm %in% c(4,5,6)  ~ "Premonsoon",
      mm %in% c(7,8,9)  ~ "Monsoon",
      mm %in% c(10,11)  ~ "Postmonsoon",
      TRUE              ~ "Winter"
    ),
    
    value = weighted_mean,
    
    predictor = case_when(
      variable == "ndvi" ~ "NDVI",
      variable == "lai"  ~ "LAI"
    )
  ) %>%
  select(site, yyyy, direction, season, predictor, value)

#===============================================================================
# 7. Combine data frames
#===============================================================================

analysis_static <- flux_seasonal %>%
  left_join(spatial_static, by = c("site", "direction"))

analysis_static <- analysis_static %>%
  mutate(predictor = predictor)

analysis_seasonal <- flux_seasonal %>%
  left_join(
    spatial_seasonal,
    by = c("site", "direction", "season", "yyyy")
  )

analysis_df <- bind_rows(
  analysis_static,
  analysis_seasonal
) %>%
  drop_na(residual, value)

#===============================================================================
# 8. Run regressions and use test size correction (bonferroni for FWER)
#===============================================================================

season_year_models <- analysis_df %>%
  group_by(site, yyyy, season, predictor) %>%
  group_modify(~{
    
    if(nrow(.x) < 5) return(tibble())
    
    m <- lm(residual ~ value, data = .x)
    
    tibble(
      r2 = summary(m)$r.squared,
      slope = coef(m)[2],
      corr = cor(.x$residual, .x$value, use = "complete.obs"),
      p = summary(m)$coefficients[2,4],
      n = nrow(.x)
    )
  }) %>%
  ungroup()

season_year_models <- season_year_models %>%
  group_by(site, yyyy, predictor) %>%
  mutate(
    p_adj = p.adjust(p, method = "bonferroni"),
    sig = p_adj < 0.05
  ) %>%
  ungroup()

#===============================================================================
# 9. Prep plot order for data/labels
#===============================================================================

season_year_models <- season_year_models %>%
  mutate(
    season = factor(season,
                    levels = c("Winter","Premonsoon","Monsoon","Postmonsoon")),
    
    site = factor(site,
                  levels = c("CMW","SRM","SRG","WKG")),
    
    predictor = factor(predictor,
                       levels = c(
                         "Topography",
                         "Canopy Cover",
                         "Canopy Height",
                         "NDVI",
                         "LAI"
                       ))
  )

season_year_models <- season_year_models %>%
  mutate(
    sig_dir = case_when(
      sig & corr > 0 ~ "positive",
      sig & corr < 0 ~ "negative",
      TRUE ~ "ns"
    )
  )
#===============================================================================
# 10. Generate Plot
#===============================================================================

ggplot(season_year_models,
       aes(x = yyyy,
           y = season,
           fill = r2)) +
  
  geom_tile(color = "gray80") +
  geom_point(
    data = subset(season_year_models, sig),
    aes(color = corr),
    size = 3
  ) +
  scale_color_gradient2(
    low = "lightblue",
    mid = "white",
    high = "pink",
    midpoint = 0,
    guide = "none"
  )+
  facet_grid(predictor ~ site) +
  
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  scale_alpha_manual(values = c(0, 1), guide = "none") +
  
  scale_x_continuous(
    breaks = seq(2005, 2020, by = 5)
  ) +
  
  labs(
    x = "",
    y = "",
    fill = expression(R^2)
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0),
    panel.spacing = unit(1.2, "lines")
  )
