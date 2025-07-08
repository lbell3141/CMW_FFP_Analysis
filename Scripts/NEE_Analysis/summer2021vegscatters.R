geos_dat <- readRDS("./Data/monthly_directional_ffps/summer2021/dfs/full_summer_frame.RDS")

#make a df of zscores for plotting for each month and direction combo

library(dplyr)

library(ggplot2)
library(gridExtra)


geos_zscores <- geos_dat %>%
  group_by(month) %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
    .names = "{.col}_z"
  )) %>%
  ungroup()
geos_zscores$direction <- as.numeric(as.character(geos_zscores$direction))
geos_zscores <- geos_zscores %>%
  arrange(month, direction)

#_-_-_-_-_-_-_-_-_-_-__-_-_-_-_-_-_-_-_-_-__-_-_-_-_-_-_-_-_-_-__-_-_-_-_-_-_-_-

# Fit linear models
lm_lai <- lm(observed_gpp_z ~ LAI_z, data = geos_zscores)
lm_cover <- lm(observed_gpp_z ~ Cover_z, data = geos_zscores)
lm_height <- lm(observed_gpp_z ~ Height_z, data = geos_zscores)

# Extract R²
r2_lai <- summary(lm_lai)$r.squared
r2_cover <- summary(lm_cover)$r.squared
r2_height <- summary(lm_height)$r.squared

# Extract p-values for slope
p_lai <- summary(lm_lai)$coefficients[2, 4]
p_cover <- summary(lm_cover)$coefficients[2, 4]
p_height <- summary(lm_height)$coefficients[2, 4]

# Significance label function
sig_label <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Plot LAI
p_lai <- ggplot(geos_zscores, aes(x = LAI_z, y = observed_gpp_z)) +
  geom_point(color = "black", size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_lai, 2), sig_label(p_lai), "\n"),
           hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "LAI", y = "Residual GPP") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Plot Cover
p_cover <- ggplot(geos_zscores, aes(x = Cover_z, y = observed_gpp_z)) +
  geom_point(color = "black", size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_cover, 2), sig_label(p_cover), "\n"),
           hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "Cover", y = "") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Plot Height
p_height <- ggplot(geos_zscores, aes(x = Height_z, y = observed_gpp_z)) +
  geom_point(color = "black", size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_height, 2), sig_label(p_height), "\n"),
           hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "Height", y = "") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Arrange plots in one row
grid.arrange(p_lai, p_cover, p_height, ncol = 3)

#===============================================================================
#===============================================================================

#_-_-_-_-_-_-_-_-_-_-__-_-_-_-_-_-_-_-_-_-__-_-_-_-_-_-_-_-_-_-__-_-_-_-_-_-_-_-

# Fit linear models
lm_lai <- lm(residual_gpp_z ~ LAI_z, data = geos_zscores)
lm_cover <- lm(residual_gpp_z ~ Cover_z, data = geos_zscores)
lm_height <- lm(residual_gpp_z ~ Height_z, data = geos_zscores)

# Extract R²
r2_lai <- summary(lm_lai)$r.squared
r2_cover <- summary(lm_cover)$r.squared
r2_height <- summary(lm_height)$r.squared

# Extract p-values for slope
p_lai <- summary(lm_lai)$coefficients[2, 4]
p_cover <- summary(lm_cover)$coefficients[2, 4]
p_height <- summary(lm_height)$coefficients[2, 4]

# Significance label function
sig_label <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Plot LAI
p_lai <- ggplot(geos_zscores, aes(x = LAI_z, y = residual_gpp_z)) +
  geom_point(color = "black", size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_lai, 2), sig_label(p_lai), "\n"),
           hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "LAI", y = "Residual GPP") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Plot Cover
p_cover <- ggplot(geos_zscores, aes(x = Cover_z, y = residual_gpp_z)) +
  geom_point(color = "black", size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_cover, 2), sig_label(p_cover), "\n"),
           hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "Cover", y = "") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Plot Height
p_height <- ggplot(geos_zscores, aes(x = Height_z, y = residual_gpp_z)) +
  geom_point(color = "black", size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_height, 2), sig_label(p_height), "\n"),
           hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "Height", y = "") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Arrange plots in one row
grid.arrange(p_lai, p_cover, p_height, ncol = 3)

#===============================================================================
#===============================================================================
#===============================================================================
#circular plots

library(ggplot2)
library(viridis)

# Ensure direction is numeric (needed for correct ordering)
geos_zscores$direction <- as.numeric(as.character(geos_zscores$direction))

# Circular bar plot, faceted by month
lai_plot <- ggplot(geos_zscores, aes(x = direction, y = 5, fill = LAI)) +
  geom_col(width = 20) +
  coord_polar(start = -pi / 2) +  # Rotate so 0° is at top (north)
  facet_wrap(~ month) +
  scale_fill_viridis_c(
    option = "viridis",
    limits = range(geos_zscores$LAI, na.rm = TRUE),
    breaks = range(geos_zscores$LAI, na.rm = TRUE),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(fill = "LAI") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(size = 13)
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3))
