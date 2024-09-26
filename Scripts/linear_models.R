#dir_dat_avg from driver_circles.R
library(ggplot2)
library(patchwork)
library(cowplot)
#renaming obj from driver_circles.R
avg_flux_dat <- dir_dat_avg
flux_dat <- dir_dat

#making linear model equation + checking r^2
#le excluded
model <- lm(gpp ~ swc + temp_atmos + rel_h + ppfd + wind_sp + precip, data=flux_dat)
r_squared <- summary(model)$r.squared

#predict data based on modeled relationship
modeled_gpp <- predict(model, flux_dat)
#add to df
flux_dat$modeled_gpp <- modeled_gpp
#find difference bt real and modeled observations 
flux_dat <- flux_dat%>%
  filter(modeled_gpp > 0)%>%
  mutate(diff_gpp = modeled_gpp - gpp)
#find average gpp value by wind direction (WD as factor to arrange in order)
flux_dat_dir <- flux_dat%>%
  group_by(dir_group)%>%
  summarize(mod_avg_gpp = mean(modeled_gpp, na.rm = T),
            real_avg_gpp = mean(gpp, na.rm = T),
            diff_avg_gpp = mean(diff_gpp, na.rm = T))
flux_dat_dir$dir_group <- factor(flux_dat_dir$dir_group, levels = seq(10, 350, 10))
flux_dat_dir <- flux_dat_dir%>%
  arrange(dir_group)


#plotting===================================
gpp_difference_annual <- ggplot(flux_dat_dir) +  
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(4, 5)), 
    color = "lightgrey"
  ) +
  geom_col(
    aes(
      x = dir_group,
      y = 5, 
      fill = diff_avg_gpp 
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
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "")+
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

gpp_difference_annual+ 
  plot_annotation(title = "Modeled - Observed Mean WD GPP")


#===============================================================================
#================================monthly modeling===============================
#===============================================================================
flux_dat <- dir_dat
mm = factor(flux_dat$mm)
mm_split_dat <- split(flux_dat, mm)

#function to make a model; predict; find difference for a given month
#same as above but with relative df, x
mod_mm <- function(x){
  model <- lm(gpp ~ swc + temp_atmos + rel_h + ppfd + wind_sp + precip + HH_UTC, data=x)
  x$modeled_gpp <- predict(model, x)
  x <- x %>%
    filter(modeled_gpp > 0)%>%
    mutate(diff_gpp = modeled_gpp - gpp)
  dir_df <- x %>%
    group_by(dir_group)%>%
    summarize(mod_avg_gpp = mean(modeled_gpp, na.rm = T),
              real_avg_gpp = mean(gpp, na.rm = T),
              diff_avg_gpp = mean(diff_gpp, na.rm = T))
  dir_df$dir_group <- factor(dir_df$dir_group, levels = seq(10, 350, 10))
  dir_df <- dir_df %>% arrange(dir_group)
  return(dir_df)
}

mm_mod_list <- lapply(mm_split_dat, mod_mm)

all_diff_avg_gpp <- unlist(lapply(mm_mod_list, function(df) df$diff_avg_gpp))
overall_min <- min(all_diff_avg_gpp, na.rm = TRUE)
overall_max <- max(all_diff_avg_gpp, na.rm = TRUE)

#plot monthly circles
plot_gpp_difference <- function(df, month) {
  ggplot(df) +  
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(4, 5)), 
      color = "lightgrey"
    ) +
    geom_col(
      aes(
        x = dir_group,
        y = 5, 
        fill = diff_avg_gpp 
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
    scale_fill_gradient2(low = "blue", 
                         mid = "white", 
                         high = "red", 
                         midpoint = 0, 
                         limits = c(overall_min, overall_max), 
                         name = "GPP Difference (Modeled - Observed)") +
    theme(
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_blank(),  
      legend.position = "none",
      text = element_text(color = "gray12", family = "Bell MT"),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(title = month)
}

#list of individual plots
plot_list <- Map(function(df, month) plot_gpp_difference(df, month), mm_mod_list, month.name)

final_plot <- wrap_plots(plot_list, ncol = 6) +
  plot_layout(guides = "collect")&
  theme(legend.position = "bottom")

final_plot + 
  plot_annotation(title = "Modeled - Observed Mean WD GPP by Month") + 
  theme(
    text = element_text(color = "gray12", family = "Bell MT")
  )

#===============================================================================
#================================monthly modeling===============================
#===============================================================================

#collapse list
#add month column using df position in list
mm_mod_list <- lapply(seq_along(mm_mod_list), function(i) {
  mm_mod_list[[i]]$month <- i
  return(mm_mod_list[[i]])
})

full_frame <- do.call(rbind, mm_mod_list)

all_real_avg_gpp <- unlist(lapply(mm_mod_list, function(df) df$real_avg_gpp))
overall_min <- min(all_real_avg_gpp, na.rm = TRUE)
overall_max <- max(all_real_avg_gpp, na.rm = TRUE)

ggplot(full_frame, aes(x = as.factor(dir_group), y = as.factor(month), fill = real_avg_gpp)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", name = "Observed GPP", limits = c(overall_min, overall_max)) +
  scale_y_discrete(labels = seq(1, 12, 1)) +
  scale_x_discrete(labels = seq(10, 360, 10)) +
  labs(title = "", x = "Direction", y = "Month", fill = "real_avg_gpp") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))



