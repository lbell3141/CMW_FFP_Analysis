#percent gapfilled GPP products by direction

library(lubridate)
library(dplyr)
library(ggplot2)

#load csv with data used in the ffp calcs
dat <- read.csv("./Data/combined_CMdata.csv", header = T)
dat$TIMESTAMP_END <- ymd_hms(as.character(dat$TIMESTAMP_END))

#add column for gapfilled observations, i.e. where USTAR < 0.2 (1 = gapfilled)
#1 = gapfilled for easier % calculation
dat_gap <- dat %>%
  mutate(FC_F = if_else(USTAR < 0.2, 1, 0))


#split into wind direction windows
deg_int <- seq(0, 360, by = 10)
skip <- 0
deg_int_real <- deg_int[!deg_int %in% skip]
split_dat <- split(dat_gap, cut(dat$WD_1_1_1, deg_int, include.lowest = TRUE, labels = deg_int_real))

for (i in seq_along(split_dat)) {
  split_dat[[i]]$dir_group <- rep(names(split_dat)[i], nrow(split_dat[[i]]))
}
dir_dat <- do.call(rbind, split_dat)

p_gf <- dir_dat %>%
  group_by(dir_group) %>%
  summarize(PERCENT_GF = sum(FC_F, na.rm = T)/n() * 100) %>%
  arrange(as.numeric(dir_group))

ggplot(data = p_gf, mapping = aes(as.numeric(dir_group), PERCENT_GF)) +
  geom_bar(stat = "identity") + 
  theme_minimal() +
  labs(x = "Wind Direction", y = "Percentage of Gapfilled Observations")
  
  
  
  


















