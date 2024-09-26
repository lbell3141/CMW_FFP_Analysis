#ANOVA for modeled and observed GPP data
#GPP ~ WD + mm + WD*mm
library(dplyr)
library(plantecophys)
library(ggplot2)
library(reshape2)
library(lubridate)
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

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

#predict gpp:

model <- lm(gpp ~ swc + temp_atmos + rel_h + ppfd + wind_sp + precip +HH_UTC, data=dir_dat)
#predict data based on modeled relationship
modeled_gpp <- predict(model, dir_dat)
#add to df
dir_dat$modeled_gpp <- modeled_gpp
#find difference bt real and modeled observations 
anova_dat <- dir_dat%>%
  filter(modeled_gpp > 0)%>%
  select(mm, dir_group, gpp, modeled_gpp)%>%
  mutate(mm = as.character(mm),
         dir_group= as.character(dir_group))
#some NAs causing plotting issues, so remove
anova_dat<- na.omit(anova_dat)
#anova test=====================================================================

obs_formula <- as.formula(gpp ~ dir_group + mm + dir_group*mm)
obs_result <- aov(obs_formula, data = anova_dat)
summary(obs_result)

mod_formula <- as.formula(modeled_gpp ~ dir_group + mm + dir_group*mm)
mod_result <- aov(mod_formula, data = anova_dat)
summary(mod_result)

#residuals==========================
#get residuals
obs_residuals <- residuals(obs_result)
mod_residuals <- residuals(mod_result)
#residual variance
obs_var <- var(obs_residuals)
mod_var <- var(mod_residuals)
obs_var
mod_var
#effect size (eta^2)==========================
#anova value results (use summary)
obs_table <- summary(obs_result)[[1]]
mod_table <- summary(mod_result)[[1]]
#obs eta^2
obs_ss_total <- sum(obs_table[, "Sum Sq"])
obs_eta_squared <- obs_table[1:3, "Sum Sq"] / obs_ss_total
#mod eta^2
mod_ss_total <- sum(mod_table[, "Sum Sq"])
mod_eta_squared <- mod_table[1:3, "Sum Sq"] / mod_ss_total
#results
obs_eta_squared
mod_eta_squared

#visualizing variance==========================
#fit values based on anova formula
obs_fitted <- fitted(obs_result)
mod_fitted <- fitted(mod_result)
#create plot_df
fitted_df <- data.frame(
  data_type = rep(c("Observed", "Modeled"), each = nrow(anova_dat)),
  anova_data = c(anova_dat$gpp, anova_dat$modeled_gpp),
  fitted_data = c(obs_fitted, mod_fitted)
)

ggplot(fitted_df, aes(x = anova_data, y = fitted_data, color = data_type)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "", x = "Input GPP", y = "Fitted GPP") +
  theme_minimal()

#CV of residuals================================
obs_res_sd <- sd(obs_residuals)
obs_mean <- mean(anova_dat$gpp)
obs_cv <- (obs_res_sd/obs_mean)*100
mod_res_sd <- sd(mod_residuals)
mod_mean <- mean(anova_dat$modeled_gpp)
mod_cv <- (mod_res_sd/mod_mean)*100
obs_cv
mod_cv

#combining info==================================
#data for plot summary:
obs_var
mod_var
obs_cv
mod_cv
#obs_eta_squared
obs_eta_squared_dir_group <- obs_eta_squared[1]
obs_eta_squared_mm <- obs_eta_squared[2]
obs_eta_squared_combo <- obs_eta_squared[3]
#mod_eta_squared
mod_eta_squared_dir_group <- mod_eta_squared[1]
mod_eta_squared_mm <- mod_eta_squared[2]
mod_eta_squared_combo <- mod_eta_squared[3]


#combine measures into a single df
comparison_df <- data.frame(
  Metric = c('eta_squared_dir_group', 'eta_squared_mm', 'eta_squared_combo','cv', 'variance'),
  Observed = c(obs_eta_squared_dir_group, obs_eta_squared_mm,obs_eta_squared_combo,obs_cv, obs_var),
  Model = c(mod_eta_squared_dir_group, mod_eta_squared_mm, mod_eta_squared_combo, mod_cv, mod_var)
)
#switch plot format for bar plotting
comparison_df <- melt(comparison_df, id.vars = "Metric")

#plotting
ggplot(comparison_df, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free") +
  labs(title = "ANOVA Stat Comparison", 
       y = "", x = "") +
  theme_minimal() +
  scale_fill_manual(values = c("Observed" = "#00BFC4", "Model" = "#F8766D")) +
  theme(legend.title = element_blank())
