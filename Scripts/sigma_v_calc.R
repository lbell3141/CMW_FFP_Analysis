#calculating sigma_v, standard deviation of lateral wind velocity fluctuations 
#equations from appendix of Luhar, 2009 (also ref Weber)


#Load data; set parameters
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$WD_radians <- dat_file$WD_1_1_1 * pi / 180

N = nrow(dat_file)

#===============================================================================
#=================Calculate Vector Average Wind Speed (x, y)====================
#===============================================================================

u_bar_x = (1/N) * sum(dat_file$WS_1_1_1 * sin(dat_file$WD_radians - pi), na.rm = T)
u_bar_y = (1/N) * sum(dat_file$WS_1_1_1 * cos(dat_file$WD_radians - pi), na.rm = T)
  
#===============================================================================
#===================Calculate Vector Average Wind Direction=====================
#===============================================================================

theta_v_bar = atan2(u_bar_x, u_bar_y) + pi

#===============================================================================
#====================Calculate Lateral Velocity Variance========================
#===============================================================================

var_sigma_v =  (1 / (N-1)) * (sum(dat_file$WS_1_1_1 * sin(theta_v_bar - dat_file$WD_radians), na.rm = T))^2

#===============================================================================
#==============Calculate Lateral Velocity Standard Deviation====================
#===============================================================================

sigma_v = sqrt(var_sigma_v)

#===============================================================================
#===============================Create Function=================================
#===============================================================================
calc_sigma_v <- function(x) {
  x$WD_radians <- x$WD_1_1_1 * pi / 180
  N <- nrow(x)
  
  u_bar_x <- (1 / N) * sum(x$WS_1_1_1 * sin(x$WD_radians - pi), na.rm = T)
  u_bar_y <- (1 / N) * sum(x$WS_1_1_1 * cos(x$WD_radians - pi), na.rm = T)
  
  theta_v_bar <- atan2(u_bar_x, u_bar_y) + pi
  
  var_sigma_v <- (1 / (N-1)) * (sum(x$WS_1_1_1 * sin(theta_v_bar - x$WD_radians), na.rm = T))^2
  sigma_v <- sqrt(var_sigma_v)
  }
