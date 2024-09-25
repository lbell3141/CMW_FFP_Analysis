#dir_dat_avg from driver_circles.R
library(ggplot2)
library(patchwork)
avg_flux_dat <- dir_dat_avg
flux_dat <- dir_dat

model <- lm(gpp ~ swc + temp_atmos + rel_h + ppfd + wind_sp + precip, data=flux_dat)
r_squared <- summary(model)$r.squared

modeled_gpp <- predict(model, flux_dat)

flux_dat$modeled_gpp <- modeled_gpp

flux_dat_dir <- flux_dat%>%
  filter(modeled_gpp > 0)%>%
  group_by(dir_group)%>%
  summarize(mod_avg_gpp = mean(modeled_gpp, na.rm = T),
            real_avg_gpp = mean(gpp, na.rm = T))
flux_dat_dir$dir_group <- factor(flux_dat_dir$dir_group, levels = seq(10, 350, 10))
flux_dat_dir <- flux_dat_dir%>%
  arrange(dir_group)

modeled_an_avg_gpp <- ggplot(flux_dat_dir) +  
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(4, 5)), 
    color = "lightgrey"
  ) +
  geom_col(
    aes(
      x = dir_group,
      y = 5, 
      fill = mod_avg_gpp 
    ),
    position = "dodge2"
  ) +
  geom_segment(
    aes(
      x = dir_group,  
      y = 5,
      xend = dir_group, 
      yend = 5
    ),
    linetype = "dashed",
    color = "white"
  ) + 
  coord_polar() +
  scale_fill_viridis_c(option = "viridis", name = "") +
  theme(
    axis.title = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text.y = element_blank(), 
    axis.text.x = element_blank(),  
    legend.position = "bottom",
    text = element_text(color = "gray12", family = "Bell MT"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(
    fill = guide_colorbar(barwidth = 8, barheight = 0.5)
  )

real_an_avg_gpp <- ggplot(flux_dat_dir) +  
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(4, 5)), 
    color = "lightgrey"
  ) +
  geom_col(
    aes(
      x = dir_group,
      y = 5, 
      fill = real_avg_gpp 
    ),
    position = "dodge2"
  ) +
  geom_segment(
    aes(
      x = dir_group,  
      y = 5,
      xend = dir_group, 
      yend = 5
    ),
    linetype = "dashed",
    color = "white"
  ) + 
  coord_polar() +
  scale_fill_viridis_c(option = "viridis", name = "") +
  theme(
    axis.title = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text.y = element_blank(), 
    axis.text.x = element_blank(),  
    legend.position = "bottom",
    text = element_text(color = "gray12", family = "Bell MT"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(
    fill = guide_colorbar(barwidth = 8, barheight = 0.5)
  )

modeled_an_avg_gpp+ 
  real_an_avg_gpp+ 
  plot_annotation(title = "Modeled vs Real Average GPP")


#===============================================================================
#================================monthly modeling===============================
#===============================================================================
