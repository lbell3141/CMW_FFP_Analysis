#footprint figures 
library(ggplot2)
library(dplyr)

hz_dat <- read.csv("./Data/combined_CMdata.csv", header = T)
hz_dat_plot <- hz_dat%>%
  select(sigma_v)%>%
  filter(!is.na(sigma_v))%>%
  filter(between(sigma_v, quantile(sigma_v, 0.01, na.rm = TRUE),
                 quantile(sigma_v, 0.99, na.rm = TRUE)))%>%
  mutate(site = "")

ggplot(hz_dat_plot, aes(x = site, y = sigma_v)) +
  geom_violin(fill = "skyblue", color = "black", alpha = 0.58) +
  stat_summary(fun = "median", geom = "point", size = 2.5, color = "darkblue") +
  theme_minimal() +
  labs(y = expression(sigma[v]), x = "", title = "")+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20))
