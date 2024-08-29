library(dplyr)
library(ggplot2)
library(boot)

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
    month %in% phenophase_periods$green_up ~ 'Green Up',
    month %in% phenophase_periods$dry_mature ~ 'Dry Mature',
    month %in% phenophase_periods$wet_mature ~ 'Wet Mature',
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
  'Green Up' = '#2ca02c',
  'Dry Mature' = '#2e8b57',
  'Wet Mature' = 'darkgreen',
  'Senescence' = '#17becf'
  )

#to plot in chronological order
avg_gpp_sum <- avg_gpp_sum %>%
  mutate(phenophase = factor(phenophase, levels = names(phenophase_colors)))

pheno_bar <- ggplot(avg_gpp_sum, aes(x = direction, y = phenophase_gpp, fill = phenophase)) +
  geom_bar(stat = "identity") +
  labs(x = "Direction", y = "Total Annual GPP (scaled monthly averages)", fill = "Phenophase") +
  scale_fill_manual(values = phenophase_colors) +
  theme_minimal()
#===============================================================================
#===============================================================================
#adding in error estimates for monthly means
#dat_voi from driver_circles.R
pheno_dat <- dat_voi %>%
  mutate(phenophase = case_when(
    mm %in% phenophase_periods$dormancy ~ 'Dormancy',
    mm %in% phenophase_periods$green_up ~ 'Green Up',
    mm %in% phenophase_periods$dry_mature ~ 'Dry Mature',
    mm %in% phenophase_periods$wet_mature ~ 'Wet Mature',
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
#boostrapping
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
calc_bootstrap_se <- function(df, n_samp = 100) {
  #bootstrap using boot::boot() and stat function from above
  result <- boot(df$gpp, bootstrap_se, n_bootstrap)
  #calc sd from generated sample means to get se (boot doesn't just have a se object in the result even though it's displayed in the console?...)
  se <- sd(result$t, na.rm = T)
  return(se)
}

#apply function across list of lists
pheno_split_se <- lapply(pheno_split_by_dir, function(x) {
  lapply(x, function(y) {
      se <- calculate_bootstrap_se(y)
      return(data.frame(standard_error = se))
    } ) })
#how does boot determine the index it uses 
#why are NAs populating


test_df <- pheno_split_by_dir_test[[1]][[7]]$gpp
boot(test_df, m, 100)


#===============================================================================
#add se to bar plot
#===============================================================================
#pheno_bar plot from above
pheno_err <- pheno_bar +
  






