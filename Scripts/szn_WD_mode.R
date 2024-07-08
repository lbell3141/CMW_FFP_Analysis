#seasonal wind direction dominance/gpp plot
library(cimir)
library(dplyr)
#load CM_dat from calc_directional_ffp.R
#find mode w charaacter from cimir func
compute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#use func
szn_dat <- CM_dat %>%
  mutate(comp_dir = cimis_degrees_to_compass(wind_dir)) %>%
  group_by(mm) %>%
  summarize(comp_dir_mode = compute_mode(comp_dir),
            gpp = mean(GPP_PI))




