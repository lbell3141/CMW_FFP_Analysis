#ANOVAs for gpp drivers and wind direction
library(patchwork)
library(ggplot2)
library(effectsize)
#===============prep df=========================================================
#split_dat from driver_circles.R 

dat_voi<- dat_file %>%
  summarize(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    wind_sp = WS_1_1_1,
    temp_atmos = TA_1_1_1,
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

#split into direction windows
deg_int <- seq(45, 360, by = 45)
deg_int_real <- deg_int
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, breaks = c(0,deg_int_real), include.lowest = F, labels = deg_int_real))
for (i in seq_along(split_dat)) {
  split_dat[[i]]$dir_group <- rep(names(split_dat)[i], nrow(split_dat[[i]]))
}
dir_dat <- do.call(rbind, split_dat)

#merge lists into a dataframe 
combd_WD <- bind_rows(split_dat)
combd_WD <- combd_WD%>%
  mutate(dir_group = as.factor(dir_group),
         mm = as.factor(mm))

#actually, split into monthly list
mwd_dat <- split(combd_WD, combd_WD$mm)

#===============run ANOVAs======================================================
#define driver variables
voi <- c("ppfd", "rel_h", "temp_atmos", "VPD", "wind_sp", "le", "swc", "gpp")

#make results df
anova_df <- data.frame(
  Month = character(),
  Variable = character(),
  F_value = numeric(),
  P_value = numeric(),
  Eta_Squared = numeric(),
  stringsAsFactors = FALSE
)

tukey_df <- data.frame(
  Month = character(),
  Variable = character(),
  Comparison = character(),
  Difference = numeric(),
  Lower_CI = numeric(),
  Upper_CI = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

#loop through month list and voi within each df
for (z in names(mwd_dat)) {
  for (i in voi) {
    #ANOVA equation
    formula <- as.formula(paste(i, "~ dir_group"))
    res_aov <- aov(formula, data = mwd_dat[[z]])
    #add vals to summary
    summary_res <- summary(res_aov)
    F_value <- summary_res[[1]][["F value"]][1]
    P_value <- summary_res[[1]][["Pr(>F)"]][1]
    eta_squared_res <- eta_squared(res_aov)
    Eta_Squared <- eta_squared_res$Eta2[1]
    #put in df
    anova_df <- rbind(anova_df, data.frame(
      Month = z,
      Variable = i,
      F_value = F_value,
      P_value = P_value,
      Eta_Squared = Eta_Squared,
      stringsAsFactors = FALSE
    ))
    
    #post hoc test for significant P
    if (P_value < 0.05) {
      tukey_res <- TukeyHSD(res_aov)
      
      
      tukey_results <- tukey_res$dir_group
#add to df
       tukey_df <- rbind(tukey_df, data.frame(
        Month = z,
        Variable = i,
        Comparison = rownames(tukey_results),
        Difference = tukey_results[, "diff"],
        Lower_CI = tukey_results[, "lwr"],
        Upper_CI = tukey_results[, "upr"],
        P_value = tukey_results[, "p adj"],
        stringsAsFactors = FALSE
      ))
    }
  }
}

anova_df
tukey_df

#plots
library(ggplot2)
anova_df <- anova_df%>%
  mutate(Month = as.numeric(Month))%>%
  arrange(Month)%>%
  mutate(Month = as.factor(Month))
  
#visualize F values: 
ggplot(anova_df, aes(x = Variable, y = F_value, fill = Month)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d()+
  theme_minimal() +
  labs(title = "x~WD F-values by Month", x = "Variable", y = "F Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#example plot from chatgpt
ggplot(tukey_df, aes(x = Comparison, y = Difference, color = Variable)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  theme_minimal() +
  labs(title = "Tukey HSD Test Results", x = "Comparison", y = "Difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#=====================================================================
#color by phenophase instead of month
#=====================================================================
phenophase_periods <- list(
  dormancy = c(1:4, 11, 12),
  green_up = 5,
  dry_mature = 6,
  wet_mature = 7:9,
  senescence = 10
)

anova_df_pheno <- anova_df %>%
  mutate(Phenophase = case_when(
    Month %in% phenophase_periods$dormancy ~ 'Dormancy',
    Month %in% phenophase_periods$green_up ~ 'Green_Up',
    Month %in% phenophase_periods$dry_mature ~ 'Dry_Mature',
    Month %in% phenophase_periods$wet_mature ~ 'Wet_Mature',
    Month %in% phenophase_periods$senescence ~ 'Senescence'
  ))
phenophase_colors <- c(
  'Dormancy' = '#1f77b4', 
  'Green_Up' = '#CFF2A8',
  'Dry_Mature' = '#86CD34',
  'Wet_Mature' = '#4E890C',
  'Senescence' = '#17becf'
)
anova_df_pheno$Month <- factor(test$Month, levels = 1:12, labels = month.name)
anova_df_pheno$Variable <- factor(test$Variable, levels = c("gpp", "wind_sp", "VPD", "temp_atmos", "rel_h", "le", "swc", "ppfd"), labels = c("GPP", "Wind Speed", "VPD", "Air Temp", "Rel. Humidity", "Latent Heat", "Soil Moisture", "PPFD"))
anova_df_pheno$Phenophase <- factor(anova_df_pheno$Phenophase, levels = c('Dormancy', 'Green_Up', 'Dry_Mature', 'Wet_Mature', 'Senescence'))
ggplot(anova_df_pheno, aes(x = Variable, y = F_value, fill = Phenophase)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Month) +
  scale_fill_manual(values = phenophase_colors) +
  scale_y_continuous(breaks = seq(0,500, 250)) +
  theme_minimal() +
  labs(title = "Monthly F Values (x ~ WD)", x = "", y = "F Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))



#=====================================================================
#=====================================================================
#=====================================================================

#CV = sd/mean
#finding seasonal (phenophase) variation 
#vision is to have CV and GPP bar graph with 5 bars colored by phenophase
#eventually have all drivers 
#add phenophases to df
phenophase_periods <- list(
  dormancy = c(1:4, 11, 12),
  green_up = 5,
  dry_mature = 6,
  wet_mature = 7:9,
  senescence = 10
)

#dir_dat from driver circles
#phenoavg_dat <- dir_dat %>%
 # mutate(Phenophase = case_when(
 #   mm %in% phenophase_periods$dormancy ~ 'Dormancy',
 #   mm %in% phenophase_periods$green_up ~ 'Green_Up',
 #   mm %in% phenophase_periods$dry_mature ~ 'Dry_Mature',
 #   mm %in% phenophase_periods$wet_mature ~ 'Wet_Mature',
 #   mm %in% phenophase_periods$senescence ~ 'Senescence'
 # )) %>%
##  group_by(Phenophase, dir_group) %>%
#  summarise(across(-c(1:6), ~ mean(.x, na.rm = TRUE)))%>%
#  ungroup()%>%
 # group_by(Phenophase)

#don't need avg and sd for WD, just per phenophase
phenocol_dat <- dir_dat %>%
  mutate(Phenophase = case_when(
    mm %in% phenophase_periods$dormancy ~ 'Dormancy',
    mm %in% phenophase_periods$green_up ~ 'Green_Up',
    mm %in% phenophase_periods$dry_mature ~ 'Dry_Mature',
    mm %in% phenophase_periods$wet_mature ~ 'Wet_Mature',
    mm %in% phenophase_periods$senescence ~ 'Senescence'
  ))%>%
  group_by(Phenophase)%>%
  summarise(
    across(-c(1:6), list(mean = ~ mean(.x, na.rm = TRUE), 
                         sd = ~ sd(.x, na.rm = TRUE)), 
           .names = "{col}_{fn}"))
#calc CV: sd/mean
cv_pheno <- phenocol_dat%>%
  summarize(across(ends_with("_sd"), ~ .x / get(sub("_sd$", "_mean", cur_column())), .names = "cv_{col}"))%>%
  mutate(Phenophase = phenocol_dat$Phenophase)%>%
  mutate(cv_dir_group_sd = NULL)
#specify phenophase as factor to plot in (temporal) order
cv_pheno$Phenophase <- factor(cv_pheno$Phenophase, levels = c(
  "Dormancy", "Green_Up", "Dry_Mature", "Wet_Mature", "Senescence"
))
#dataframe format messy- maybe fix later
#plot cv
phenophase_colors <- c(
  'Dormancy' = '#1f77b4', 
  'Green_Up' = '#CFF2A8',
  'Dry_Mature' = '#86CD34',
  'Wet_Mature' = '#4E890C',
  'Senescence' = '#17becf'
)

#define plot labels
xaxlab = "Phenophase"
yaxlab = "Coefficient of Variation"
plotlabs = c("Wind Speed", "Air Temperature", "Friction Velocity", "Wind Direction", "GPP", "Precipitation", "Relative Humidity", "VPD", "PPFD", "Latent Heat", "Soil Moisture")

#use function to loop through driver columns
plot_cvs <- function(z, plotlab) {
  ggplot(data = cv_pheno, aes_string(x = "Phenophase", y = z, fill = "Phenophase")) +
    geom_bar(stat = "identity") +
    labs(fill = "Phenophase") +
    scale_fill_manual(values = phenophase_colors) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.position = "none") +
    ggtitle(plotlab)
}

#define columns for plot
plot_cols <- grep("^cv_", names(cv_pheno), value = TRUE)
#apply function to cols
cv_plots <- mapply(plot_cvs, plot_cols, plotlabs, SIMPLIFY = FALSE)
#format plots in grid and add in legend
design <- "
ABC
DEF
GHI
JKL
"

combined_plot <- wrap_plots(cv_plots, design = design) + 
  plot_layout(guides = 'collect') +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    plot.margin = margin(20, 20, 20, 20)
  ) +
  plot_annotation(
    title = "Coefficient of Variation by Phenophase",
    subtitle = NULL,
    caption = NULL,
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

combined_plot
#================================================================================
#================================================================================
#same thing but monthly
#mean for wind direction vals by month: remove sampling bias by making all just one observation
mmdir_dat <- dir_dat %>%
  group_by(mm, dir_group) %>%
  summarise(
    across(c(wind_sp, temp_atmos, u_star, wind_dir, gpp, precip, rel_h, VPD, ppfd, le, swc), 
           list(mean = ~ mean(.x, na.rm = TRUE)), 
           .names = "{col}_{fn}")
  )
#calc monthly mean and sd 
mm_stat <- mmdir_dat%>%
  group_by(mm)%>%
  summarise(
    across(c(2:12), list(mean = ~ mean(.x, na.rm = TRUE), 
                         sd = ~ sd(.x, na.rm = TRUE)), 
           .names = "{col}_{fn}"))

#calc CV: sd/mean
cv_mm <- mm_stat%>%
  summarize(across(ends_with("_sd"), ~ .x / get(sub("_sd$", "_mean", cur_column())), .names = "cv_{col}"))%>%
  mutate(Month = seq(1,12,1)) %>%
  mutate(Phenophase = c("Dormancy","Dormancy","Dormancy","Dormancy","Green_Up","Dry_Mature", "Wet_Mature","Wet_Mature","Wet_Mature","Senescence", "Dormancy","Dormancy"))

#reduce variables plotted
#col names off but doesn't matter for plotting so I'm not fixing it
cv_mm <- cv_mm[, c(1:2, 5:13)]
#specify phenophase as factor to plot in (temporal) order

#define plot labels
plotlabs = c("Wind Speed", "Air Temperature", "GPP", "Precipitation", "Relative Humidity", "VPD", "PPFD", "Latent Heat", "Soil Moisture")
#y axis breaks
yax_breaks = seq(0, 2.5, by = 0.5)

#use function to loop through driver columns
plot_cvs <- function(z, plotlab) {
  ggplot(data = cv_mm, aes_string(x = "Month", y = z, fill = "Phenophase")) +
    geom_bar(stat = "identity") +
    labs(fill = "Phenophase") +
    scale_fill_manual(values = phenophase_colors) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_y_continuous(breaks = yax_breaks,limits = c(0, 2.5))+
    ggtitle(plotlab)
}

#define columns for plot
plot_cols <- grep("^cv_", names(cv_mm), value = TRUE)
#apply function to cols
cv_plots <- mapply(plot_cvs, plot_cols, plotlabs, SIMPLIFY = FALSE)
#format plots in grid and add in legend
design <- "
ABC
DEF
GHI
"

combined_plot <- wrap_plots(cv_plots, design = design) + 
  plot_layout(guides = 'collect') +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    plot.margin = margin(20, 20, 20, 20)
  ) +
  plot_annotation(
    title = "Coefficient of Variation by Month",
    subtitle = NULL,
    caption = NULL,
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

combined_plot



#==============================
#==============================
# Calculate mean for selected columns grouped by month and direction group
mmdir_dat <- dir_dat %>%
  group_by(mm, dir_group) %>%
  summarise(across(c(wind_sp, temp_atmos, u_star, wind_dir, gpp, precip, rel_h, VPD, ppfd, le, swc), 
                   mean, na.rm = TRUE, 
                   .names = "{col}_mean"))

# Calculate monthly mean and standard deviation for the selected variables
mm_stat <- mmdir_dat %>%
  group_by(mm) %>%
  summarise(across(ends_with("_mean"), 
                   list(mean = ~ mean(.x, na.rm = TRUE), 
                        sd = ~ sd(.x, na.rm = TRUE)), 
                   .names = "{col}_{fn}"))

cv_mm_stat <- mm_stat %>%
  mutate(across(ends_with("_sd"), 
                ~ .x / get(sub("_sd$", "_mean", cur_column())), 
                .names = "cv_{col}"))
cv_mm_stat <- cv_mm_stat[,c(1,25, 28:34)]
cv_mm_stat <- cv_mm_stat%>%
  mutate(Phenophase = c("Dormancy","Dormancy","Dormancy","Dormancy","Green_Up","Dry_Mature", "Wet_Mature","Wet_Mature","Wet_Mature","Senescence", "Dormancy","Dormancy"))
cv_mm_stat$Phenophase <- factor(cv_mm_stat$Phenophase, levels = c('Dormancy', 'Green_Up', 'Dry_Mature', 'Wet_Mature', 'Senescence'))
#define plot labels
plotlabs = c("GPP", "Latent Heat","Air Temperature","Relative Humidity", "VPD",   "Soil Moisture","PPFD")
cv_mm_stat <- cv_mm_stat%>%
  select(mm, Phenophase, cv_gpp_mean_sd, cv_le_mean_sd, cv_temp_atmos_mean_sd, cv_rel_h_mean_sd, cv_VPD_mean_sd, cv_swc_mean_sd, cv_ppfd_mean_sd)

#y axis breaks
yax_breaks = seq(0, 0.6, by = 0.2)

#use function to loop through driver columns
plot_cvs <- function(z, plotlab) {
  ggplot(data = cv_mm_stat, aes_string(x = "mm", y = z, fill = "Phenophase")) +
    geom_bar(stat = "identity") +
    labs(fill = "Phenophase", y = "Coefficient of Variation") +
    scale_fill_manual(values = phenophase_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 65, hjust = 1),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.position = "none") + 
    scale_y_continuous(breaks = yax_breaks, limits = c(0, 0.6)) +
    scale_x_continuous(breaks = seq(1,12,1), labels = month.abb) + 
    ggtitle(plotlab)
}

#define columns for plot
plot_cols <- grep("^cv_", names(cv_mm_stat), value = TRUE)
#apply function to cols
cv_plots <- mapply(plot_cvs, plot_cols, plotlabs, SIMPLIFY = FALSE)
#format plots in grid and add in legend
design <- "
ABC
DEF
GHI
"

combined_plot <- wrap_plots(cv_plots, design = design) + 
  plot_layout(guides = 'collect') +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    plot.margin = margin(20, 20, 20, 20)
  ) +
  plot_annotation(
    title = "Monthly Coefficient of Variation",
    subtitle = NULL,
    caption = NULL,
    theme = theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

combined_plot
