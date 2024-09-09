
#======with quantiles instead of se=
calc_scaled_bootstrap_quantiles <- function(df, n_samp = 100, scale_factor, probs = c(0.025, 0.975)) {
  result <- boot(df$gpp, calc_mean, n_samp)
  scaled_means <- result$t * scale_factor
  avg_scaled_gpp <- mean(scaled_means, na.rm = TRUE)
  quantiles <- quantile(scaled_means, probs = probs, na.rm = TRUE)
  
  #quantiles and avg scaled GPP
  return(data.frame(
    quantile_lower = quantiles[1],
    quantile_upper = quantiles[2],
    scaled_avg = avg_scaled_gpp
  ))
}

#apply function
pheno_split_quantiles_scaled <- mapply(function(season_list, scale_factor) {
  lapply(season_list, function(direction_df) {
    calc_scaled_bootstrap_quantiles(direction_df, scale_factor = scale_factor)
  })
}, pheno_split_by_dir_test, season_lengths, SIMPLIFY = FALSE)



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

#============================plotting===========================================
#pivot frame for phenophase columns
wide_pheno <- avg_gpp_sum %>%
  pivot_wider(names_from = phenophase, values_from = phenophase_gpp)%>%
  mutate(direction = as.numeric(as.character(direction)))

#reformat se from list to df
pheno_err <- map_df(names(pheno_split_quantiles_scaled), function(outer_name) {
  inner_list <- pheno_split_quantiles_scaled[[outer_name]]
  
  map_df(names(inner_list), function(inner_name) {
    inner_list[[inner_name]] %>%
      mutate(Phenophase = outer_name, Direction = inner_name)
  })
})

#make col per phenophase
pheno_err_wide <- pheno_err %>%
  mutate(scaled_avg = NULL)%>%
  pivot_wider(names_from = Phenophase, values_from = c(quantile_lower, quantile_upper))%>%
  mutate(Direction = as.numeric(Direction))

#join se df with phenophase avg gpp df
pheno_with_se <- wide_pheno %>%
  left_join(pheno_err_wide, by = c("direction" = "Direction"))

#plot:
yax_breaks <- seq(0, 1800, by = 300)
xax_breaks <- seq(10,360, by = 20)

dormancy <- ggplot(data = pheno_with_se, aes(x = direction, y = Dormancy))+
  geom_bar(stat = "identity", fill = "#1f77b4") +
  geom_errorbar(aes(ymin = quantile_lower_Dormancy, ymax = quantile_upper_Dormancy),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Dormancy GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  #scale_x_continuous(breaks = xax_breaks,limits = c(0, 360))+
  theme_minimal()
gr_up <- ggplot(data = pheno_with_se, aes(x = direction, y = Green_Up))+
  geom_bar(stat = "identity", fill = "#2ca02c") +
  geom_errorbar(aes(ymin = quantile_lower_Green_Up, ymax = quantile_upper_Green_Up),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Green Up GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  theme_minimal()
dry_mat <- ggplot(data = pheno_with_se, aes(x = direction, y = Dry_Mature))+
  geom_bar(stat = "identity", fill = "#2e8b57") +
  geom_errorbar(aes(ymin = quantile_lower_Dry_Mature, ymax = quantile_upper_Dry_Mature),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Dry Mature GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  theme_minimal()
wet_mat <- ggplot(data = pheno_with_se, aes(x = direction, y = Wet_Mature))+
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_errorbar(aes(ymin = quantile_lower_Wet_Mature, ymax = quantile_upper_Wet_Mature),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Wet Mature GPP") +
  scale_y_continuous(breaks = yax_breaks,limits = c(0, 1800))+
  theme_minimal()
senes <- ggplot(data = pheno_with_se, aes(x = direction, y = Senescence))+
  geom_bar(stat = "identity", fill = "#17becf") +
  geom_errorbar(aes(ymin = quantile_lower_Senescence, ymax = quantile_upper_Senescence),
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

#===========================square and add variance to composite plot
#pheno_err from annual_avg_phenophase.R
sq_pheno_err <- pheno_err %>%
  mutate(scaled_avg = NULL)%>%
  mutate(variance = (standard_error)^2)%>%
  group_by(Direction)%>%
  mutate(dir_se = sqrt(sum(variance)))%>%
  rename(direction = Direction)%>%
  rename(phenophase = Phenophase)%>%
  mutate(direction = as.numeric(direction))

avg_gpp_sum <- left_join(avg_gpp_sum, sq_pheno_err, by = c("direction", "phenophase"))

avg_gpp_sum <- avg_gpp_sum %>%
  mutate(phenophase = factor(phenophase, levels = names(phenophase_colors)))

pheno_bar <- ggplot(avg_gpp_sum, aes(x = direction, y = phenophase_gpp, fill = phenophase)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = total_gpp - dir_se, ymax = total_gpp + dir_se),
                width = 0.2, color = "black") +
  labs(x = "Direction", y = "Total Annual GPP", fill = "Phenophase") +
  scale_fill_manual(values = phenophase_colors) +
  theme_minimal() 

