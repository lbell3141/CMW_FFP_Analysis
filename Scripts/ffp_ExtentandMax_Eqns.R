#testing footprint extent formula from Kljun 2015

#x_R equn on pg 3702
#x_r = 
#  (((-c)/(ln(R))) + d) *
#  z_m * ((1 - (z_m / h))^(-1)) *
#  ((u_bar * z_m) / u_star) * k

#x_max equn on pg 3701
#x_max = 
#  0.87 *
#  z_m * ((1 - (z_m / h))^(-1)) *
#  ((u_bar * z_m) / u_star) * k


#given parameters (pg 3700)
#a = 1.452
#b = −1.991
#c = 1.462
#d = 0.136 

test <- mean(dat_voi$u_star, na.rm = T)



R = 0.9
c = 1.462
d = 0.136
z_m = 7
h = 1000
u_bar = 1.47091
u_star =  test
k = 0.4

#distance from the tower that the R% contour samples
x_R = 
  (((-c)/(log(R))) + d) *
  z_m * ((1 - (z_m / h))^(-1)) *
  ((u_bar * z_m) / u_star) * k
x_R

#x_max distance from the tower to max contribution to the measured flux
x_max = 
  0.87 *
  z_m * ((1 - (z_m / h))^(-1)) *
  ((u_bar * z_m) / u_star) * k
x_max
