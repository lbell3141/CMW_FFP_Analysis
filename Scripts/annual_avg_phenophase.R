library(dplyr)
library(purrr)
library(ggplot2)
library(boot)
library(tidyr)
library(ggpubr)

#simulated annual sum gpp 

#dir_dat_avg from driver_circles.R
avg_gpp <- dir_dat_avg[, c(1,2,7)]
avg_gpp <- avg_gpp%>%
  rename(
    direction = dir_group,
    month = mm
  )

#number of days for each month in order (jan to dec) to use to mutate the df
mm_length <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

avg_gpp <- avg_gpp %>%
  mutate(month_gpp = gpp * mm_length[month])%>%
  group_by(direction)%>%
  mutate(yy_gpp = sum(month_gpp))

plot(avg_gpp$direction, avg_gpp$yy_gpp)  

#add phenophase contribution to plot
#gallo et al 2024 phenophase DOY approximated monthly as
#Dormancy: c(1:4, 11, 12), green up: 5, dry mature: 6, wet mature: 7:9, senescence: 10
phenophase_periods <- list(
  dormancy = c(1:4, 11, 12),
  green_up = 5,
  dry_mature = 6,
  wet_mature = 7:9,
  senescence = 10
)

#add phenophase groupings to df
avg_gpp <- avg_gpp %>%
  mutate(phenophase = case_when(
    month %in% phenophase_periods$dormancy ~ 'Dormancy',
    month %in% phenophase_periods$green_up ~ 'Green_Up',
    month %in% phenophase_periods$dry_mature ~ 'Dry_Mature',
    month %in% phenophase_periods$wet_mature ~ 'Wet_Mature',
    month %in% phenophase_periods$senescence ~ 'Senescence'
  ))

#calc gpp for each phenophase for each direction
avg_gpp_sum <- avg_gpp %>%
  group_by(direction, phenophase) %>%
  summarise(phenophase_gpp = sum(month_gpp), .groups = 'drop')

#then make separate df for total gpp by direction
total_gpp <- avg_gpp_sum %>%
  group_by(direction) %>%
  summarise(total_gpp = sum(phenophase_gpp), .groups = 'drop')

#find phenophase % contribution to total gpp
avg_gpp_sum <- avg_gpp_sum %>%
  left_join(total_gpp, by = "direction")

#plotting
#colors for phenophases
phenophase_colors <- c(
  'Dormancy' = '#1f77b4', 
  'Green_Up' = '#2ca02c',
  'Dry_Mature' = '#2e8b57',
  'Wet_Mature' = 'darkgreen',
  'Senescence' = '#17becf'
  )

#to plot in chronological order
avg_gpp_sum <- avg_gpp_sum %>%
  mutate(phenophase = factor(phenophase, levels = names(phenophase_colors)))

pheno_bar <- ggplot(avg_gpp_sum, aes(x = direction, y = phenophase_gpp, fill = phenophase)) +
  geom_bar(stat = "identity") +
  labs(x = "Direction", y = "Total Annual GPP", fill = "Phenophase") +
  scale_fill_manual(values = phenophase_colors) +
  theme_minimal() 
  
  #theme(legend.position = "none")


#===============================================================================
#===============================================================================
#adding in error estimates for monthly means
#dat_voi from driver_circles.R
pheno_dat <- dat_voi %>%
  mutate(phenophase = case_when(
    mm %in% phenophase_periods$dormancy ~ 'Dormancy',
    mm %in% phenophase_periods$green_up ~ 'Green_Up',
    mm %in% phenophase_periods$dry_mature ~ 'Dry_Mature',
    mm %in% phenophase_periods$wet_mature ~ 'Wet_Mature',
    mm %in% phenophase_periods$senescence ~ 'Senescence'
  ))
#list for phenophases
pheno_split <- split(pheno_dat, pheno_dat$phenophase)
#then split by direction
deg_int <- seq(10, 350, by = 10)
deg_int_real <- deg_int

#split phenophase df by WD
pheno_split_by_dir <- lapply(pheno_split, function(df) {
  split(df, cut(df$wind_dir, breaks = c(0, deg_int, 360), include.lowest = TRUE, labels = c(deg_int_real, 360)))
})

#================================
#boostrapping phenophases
#================================
#boot() requires data, stat function, and a specification of the number of samples to take
#data (list) generated above
#define function for mean:

#NAs are messing with calc even when specified to be removed. Removing NAs manually
rm_nas <- function(df) {
  df %>% filter(!is.na(gpp))
}
pheno_split_by_dir_test <- lapply(pheno_split_by_dir, function(x) {
  lapply(x, function(i) {
    rm_nas(i)
  })
})

#define function for mean:
calc_mean <- function(data, i) {
  mean(data[i], na.rm = T)
  }

#function to deal with list of lists with dfs
#calc avg bootstrapped phenophase gpp; scale to num days in phenophase (def below)
calc_scaled_bootstrap_se <- function(df, n_samp = 100, scale_factor) {
  result <- boot(df$gpp, calc_mean, n_samp)
  scaled_means <- result$t * scale_factor
  avg_scaled_gpp <- mean(scaled_means, na.rm = TRUE)
  se <- sd(scaled_means, na.rm = T)
  return(data.frame(standard_error = se, scaled_avg = avg_scaled_gpp))
}

#phenophase month lengths for scaling
season_lengths <- c(181, 30, 31, 31, 92)

#apply both functions to pheno list 
pheno_split_se_scaled <- mapply(function(season_list, scale_factor) {
  lapply(season_list, function(direction_df) {
    calc_scaled_bootstrap_se(direction_df, scale_factor = scale_factor)
  })
}, pheno_split_by_dir_test, season_lengths, SIMPLIFY = FALSE)


#======================================
#boostrapping for simulated annual total
#======================================
#bootstrap split_dat to calculate many avg GPP fluxes and scale those fluxes by year (365days) to create variation in sums for each direction
#remove NA GPP values just in case 
split_dat_cln <- lapply(split_dat, rm_nas)
#bootstrap function for mean from above: calc_mean
calc_scaled_sc <- function(df, n_samp = 100){
  result <- boot(df$gpp, function(data, i){
    avg_day_gpp <- mean(data[i], na.rm =T)
    scaled_avg <- avg_day_gpp *365
    return(scaled_avg)
  }, n_samp)
  se <- sd(result$t, na.rm = T)
  return(data.frame(standard_error = se, scaled_avg = mean(result$t, na.rm = T)))
}

total_an_se <- lapply(split_dat_cln, calc_scaled_sc)

#===============================================================================
#add se to bar plot
#===============================================================================
#pivot frame for phenophase columns
wide_pheno <- avg_gpp_sum %>%
  pivot_wider(names_from = phenophase, values_from = phenophase_gpp)%>%
  mutate(direction = as.numeric(as.character(direction)))

#reformat se from list to df
pheno_err <- map_df(names(pheno_split_se_scaled), function(outer_name) {
  inner_list <- pheno_split_se_scaled[[outer_name]]
  
  map_df(names(inner_list), function(inner_name) {
    inner_list[[inner_name]] %>%
      mutate(Phenophase = outer_name, Direction = inner_name)
  })
})

#make col per phenophase
pheno_err_wide <- pheno_err %>%
  mutate(scaled_avg = NULL)%>%
  pivot_wider(names_from = Phenophase, values_from = standard_error)%>%
  rename_with(~paste0(., "_se"), -1)%>%
  mutate(Direction = as.numeric(Direction))

#join se df with phenophase avg gpp df
pheno_with_se <- wide_pheno %>%
  left_join(pheno_err_wide, by = c("direction" = "Direction"))

#plot:
yax_breaks <- seq(0, 1800, by = 300)
xax_breaks <- seq(10,360, by = 20)

dormancy <- ggplot(data = pheno_with_se, aes(x = direction, y = Dormancy))+
  geom_bar(stat = "identity", fill = "#1f77b4") +
  geom_errorbar(aes(ymin = Dormancy - Dormancy_se, ymax = Dormancy + Dormancy_se),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Dormancy GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  #scale_x_continuous(breaks = xax_breaks,limits = c(0, 360))+
  theme_minimal()
gr_up <- ggplot(data = pheno_with_se, aes(x = direction, y = Green_Up))+
  geom_bar(stat = "identity", fill = "#2ca02c") +
  geom_errorbar(aes(ymin = Green_Up - Green_Up_se, ymax = Green_Up + Green_Up_se),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Green Up GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  theme_minimal()
dry_mat <- ggplot(data = pheno_with_se, aes(x = direction, y = Dry_Mature))+
  geom_bar(stat = "identity", fill = "#2e8b57") +
  geom_errorbar(aes(ymin = Dry_Mature - Dry_Mature_se, ymax = Dry_Mature + Dry_Mature_se),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Dry Mature GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  theme_minimal()
wet_mat <- ggplot(data = pheno_with_se, aes(x = direction, y = Wet_Mature))+
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_errorbar(aes(ymin = Wet_Mature - Wet_Mature_se, ymax = Wet_Mature + Wet_Mature_se),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Wet Mature GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  theme_minimal()
senes <- ggplot(data = pheno_with_se, aes(x = direction, y = Senescence))+
  geom_bar(stat = "identity", fill = "#17becf") +
  geom_errorbar(aes(ymin = Senescence - Senescence_se, ymax = Senescence + Senescence_se),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Senescence GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  theme_minimal()


#add plots together
pheno_plots <- ggarrange(dormancy, gr_up, dry_mat, NULL, wet_mat, senes, pheno_bar,NULL,
                         ncol = 4,
                         nrow = 2, 
                         widths = c(1,1,1,0.1))


pheno_plots


#=============================Moisture separated phases=========================
#top of soil profile