#seasonal wind direction dominance/gpp plot
library(cimir)
library(dplyr)
library(igraph)
library(ggplot2)
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

#convert compass directions to generic angles
#mathematical conversion not working; define angles by hand
#add pi/2 to start from compass 0 instead of standard coord plane 0
direction_to_angle <- function(direction) {
  directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  angles <- c(pi/2, 3*pi/8, pi/4, pi/8, 0, -pi/8, -pi/4, -3*pi/8, 3*pi/2, 5*pi/4, 7*pi/8, 5.8*pi/4, pi, 5*pi/8, 7*pi/8, 5*pi/8)
  angle <- angles[match(direction, directions)]
  return(angle)
}

#calc arrow positions 
szn_dat <- szn_dat %>%
  mutate(angle = sapply(comp_dir_mode, direction_to_angle),
         xend = mm + 0.5 * cos(angle),
         yend = gpp + 0.5 * sin(angle))

#make plot
plot(szn_dat$mm, szn_dat$gpp, xlab = "Month", ylab = "Average GPP", pch = 16)
#add arrows to plot
arrows(x0 = szn_dat$mm, y0 = szn_dat$gpp, x1 = szn_dat$xend, y1 = szn_dat$yend,
       length = 0.1, angle = 20, col = "black")



