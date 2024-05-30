#compilation of average driver values for increments of 10 degrees around CMW

library(lubridate)
library(dplyr)
library(plantecophys)
library(ggplot2)
library(report)
library(effectsize)
library(multcomp)
library(gridExtra)

#Load data; set parameters
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

dat_file$VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA)

meas_h <- 14
d <- (2/3) * meas_h
bound_h <- 1000

dat_voi = dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h,
    d = d, 
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    wind_sp = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    temp_atmos = TA_1_1_1,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L,
    #adding associated fluxes
    gpp = GPP_PI,
    nee = NEE_PI,
    reco = RECO_PI,
    le = LE,
    ppfd = PPFD_IN_PI_F,
    precip = P,
    rel_h = RH_1_1_1,
    swc = SWC_PI_1_1_A,
    VPD = VPD
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.2)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip, rel_h, swc, VPD)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0) 
dat_voi$L = as.numeric(dat_voi$L)
#dat_voi$sigma_v = as.numeric(dat_voi$sigma_v)
dat_voi$u_star = as.numeric(dat_voi$u_star)
dat_voi$wind_dir = as.numeric(dat_voi$wind_dir)

#===============================================================================
#===================GPP Driver Box Plots (Indv and Stacked)=====================
#===============================================================================

deg_int <- seq(0, 360, by = 10)
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))

dir_names <- paste0(deg_int[-length(deg_int)], "-", deg_int[-1])
names(split_dat) <- dir_names

#convert list to a df
#give each a list ID
split_dat_ID <- lapply(seq_along(split_dat), function(i) {
  split_dat_ID <- split_dat[[i]]
  split_dat_ID$WD <- dir_names[i]
  return(split_dat_ID)
})
#merge lists into a dataframe 
combd_WD <- bind_rows(split_dat_ID)

#create boxplots
#define driver variables
voi <- c("ppfd", "VPD", "wind_sp", "le", "swc", "gpp")

#loop through each variable in voi and plot: define axes, unquote voi, define type of plot, adjust plot visuals
#make WD a factor so it increased correctly as levels
combd_WD$WD <- factor(combd_WD$WD, levels = dir_names)

plots <- lapply(voi, function(obj) {
    ggplot(data = combd_WD, aes(x = WD, y = !!sym(obj))) +
    geom_boxplot() +
    labs(
      #title = paste("Boxplot of", obj, "by Wind Direction"),
         x = "", y = obj) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "red")
})
plots
do.call(grid.arrange, c(plots, ncol = 1))
#===============================================================================
#==============================ANOVA Tests======================================
#===============================================================================
#define variables to loop through and make dataframe for results
voi <- c("ppfd", "rel_h", "temp_atmos", "VPD", "wind_sp", "le", "swc")
anova_df <- data.frame(Variable = character(),
                       F_value = numeric(),
                       P_value = numeric(),
                       Eta_Squared = numeric(),
                       stringsAsFactors = FALSE
                       )
#loop through vars and store results in df
for (i in voi){
  formula <- as.formula(paste(i, "~ WD"))
  res_aov <- aov(formula, data = combd_WD)
  
  # Extract F-value and p-value from ANOVA summary
  summary_res <- summary(res_aov)
  F_value <- summary_res[[1]][["F value"]][1]
  P_value <- summary_res[[1]][["Pr(>F)"]][1]
  
  # Calculate Eta Squared
  eta_squared_res <- eta_squared(res_aov)
  Eta_Squared <- eta_squared_res$Eta2[1]
  
  # Append the results to the data frame
  anova_df <- rbind(anova_df, data.frame(
    Variable = i,
    F_value = F_value,
    P_value = P_value,
    Eta_Squared = Eta_Squared,
    stringsAsFactors = FALSE
  ))
}

voi <- c("ppfd", "VPD", "wind_sp", "le", "swc")

#post hoc Tukey HSD test
post_hoc_results <- list()
for (i in voi){
  #post hoc Tukey test
  post_test <- glht(res_aov, linfct = mcp(WD = "Tukey"))
  
  #results
  post_hoc_results[[i]] <- summary(post_test)
}

#make anova model
res_aov <- aov(VPD ~ WD, data = combd_WD)
#perform post hoc test
tukey_results <- TukeyHSD(res_aov, conf.level = 0.95)
#plot results? doesn't seem v helpful
plot(TukeyHSD(res_aov, conf.level = 0.95), las = 2)

#make into df and filter for values of sig dif
tukey_df <- as.data.frame(tukey_results$WD)
significant_pairs <- tukey_df[tukey_df$`p adj` < 0.05, ]
#plot(significant_pairs$diff)
#===============================================================================
#==============================Multi-var ANOVA==================================
#===============================================================================
res.manova <- manova(cbind(ppfd, VPD, swc) ~ WD, data = combd_WD)
summary(res.manova)
report(res.manova)








