#ANOVAs for gpp drivers and wind direction

#===============prep df=========================================================
#split_dat from driver_circles.R 
dir_names <- paste0(deg_int[-length(deg_int)], "-", deg_int[-1])
names(split_dat) <- dir_names

split_dat_ID <- lapply(seq_along(split_dat), function(i) {
  split_dat_ID <- split_dat[[i]]
  split_dat_ID$WD <- dir_names[i]
  return(split_dat_ID)
})
#merge lists into a dataframe 
combd_WD <- bind_rows(split_dat_ID)

combd_WD <- combd_WD %>%
  filter(mm == 8)
 # filter(doy %in% 194:281)%>%
  #filter(HH_UTC %in% 9:12)


#===============run ANOVAs======================================================
#define driver variables
voi <- c("ppfd", "rel_h", "temp_atmos", "VPD", "wind_sp", "le", "swc", "gpp")

#loop through each variable in voi and plot: define axes, unquote voi, define type of plot, adjust plot visuals
#make WD a factor so it increased correctly as levels
combd_WD$dir_group <- factor(combd_WD$WD, levels = dir_names)


#loop through voi and make dataframe colunms for results
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
  
  #store F-value and p-value from summary
  summary_res <- summary(res_aov)
  F_value <- summary_res[[1]][["F value"]][1]
  P_value <- summary_res[[1]][["Pr(>F)"]][1]
  
  #calc e^2
  eta_squared_res <- eta_squared(res_aov)
  Eta_Squared <- eta_squared_res$Eta2[1]
  
  #add e^2 to df
  anova_df <- rbind(anova_df, data.frame(
    Variable = i,
    F_value = F_value,
    P_value = P_value,
    Eta_Squared = Eta_Squared,
    stringsAsFactors = FALSE
  ))
}

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
phenoavg_dat <- dir_dat %>%
  mutate(Phenophase = case_when(
    mm %in% phenophase_periods$dormancy ~ 'Dormancy',
    mm %in% phenophase_periods$green_up ~ 'Green_Up',
    mm %in% phenophase_periods$dry_mature ~ 'Dry_Mature',
    mm %in% phenophase_periods$wet_mature ~ 'Wet_Mature',
    mm %in% phenophase_periods$senescence ~ 'Senescence'
  )) %>%
  group_by(Phenophase, dir_group) %>%
  summarise(across(-c(1:6), ~ mean(.x, na.rm = TRUE)))%>%
  ungroup()%>%
  group_by(Phenophase)

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
  mutate(Phenophase = phenocol_dat$Phenophase)
