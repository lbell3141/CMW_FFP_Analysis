#compare drivers of gpp by creating directional (circular) plots with driver values normalized by month
#drivers considered: atmos temp, rel humidity, VPD, latent heat, soil water, wind speed, ppfd

library(lubridate)
library(dplyr)
library(plantecophys)

library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)
library(patchwork)

#load avg raster data made in geospatial_mm_ffp.R
PathToMmDat <- "./Data/monthly_directional_ffps/raster_dataframes/FullRasterFrame.rds"
mm_dat <- readRDS(PathToMmDat)

#load flux data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))
#and canopy cover data for combining below
month_cc_df <- readRDS("./Data/monthly_directional_ffps/raster_dataframes/month_cc.RDS")

#plotting normalized values, so create a function for zscores: 
calc_z_score <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}



#create dataframe
dat_voi<- dat_file %>%
  summarize(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    wind_sp = WS_1_1_1,
    temp_atmos = TA_1_1_1,
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17) %>%
  #group by month and calculate zscores (based on monthly values that is)
  group_by(mm) %>%
  mutate(
    wind_sp_mzs = calc_z_score(wind_sp),
    temp_atmos_mzs = calc_z_score(temp_atmos),
    rel_h_mzs = calc_z_score(rel_h),
    VPD_mzs = calc_z_score(VPD),
    ppfd_mzs = calc_z_score(ppfd),
    le_mzs = calc_z_score(le),
    swc_mzs = calc_z_score(swc),
    gpp_mzs = calc_z_score(gpp)
  ) %>%
  ungroup()


#create dataframe
dat_voi<- dat_file %>%
  summarize(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    wind_sp = WS_1_1_1,
    temp_atmos = TA_1_1_1,
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

#split into direction windows
deg_int <- seq(0, 360, by = 10)
deg_int_label <- seq(10, 360, by = 10)
dat_voi$dir_group <- cut(dat_voi$wind_dir, breaks = deg_int, include.lowest = T, labels = deg_int_label)
split_dat <- split(dat_voi, dat_voi$dir_group)
dir_dat <- do.call(rbind, split_dat)

#calc avg normalized value for each direction group in each month
#time averages aren't correct with this but we don't need for plotting so I'm not fixing it- just fyi
dir_dat_avg <- dir_dat %>%
  group_by(mm, dir_group)%>%
  summarise_all(~mean(.x, na.rm = T)) %>%
  mutate(dir_group = as.numeric(dir_group))%>%
  arrange(mm, dir_group)
dir_dat_avg = dir_dat_avg[,c(1:2,8:18)]
dir_dat_avg <- dir_dat_avg %>%
  #filter(dir_group != 50)%>%
  rename(month = mm)%>%
  rename(direction = dir_group)

#add canopy data from monthly_canopy_cover.R
plot_df <-inner_join(dir_dat_avg, month_cc_df, by = c("month", "direction"))
plot_df <- plot_df%>%
  rename(canopy_cover = veg_cover) %>%
  mutate(canopy_cover = canopy_cover * 100 - 50)

#need to manipulate entire structure of dataframe (so super cool!) to have month-variables instead of separate columns
month_split <- split(plot_df, plot_df$month)
#add month abbreviations to all variable column names
month_abbr <- month.abb

#loop through and rename
month_split <- lapply(1:12, function(i) {
  df <- month_split[[i]]
  
  #create new column names for variables, but don't rename direction or month column
  new_colnames <- names(df)
  new_colnames <- ifelse(new_colnames != c("month", "direction"), 
                         paste0(month_abbr[i], "_", new_colnames),
                         new_colnames)
  
  #apply names
  df <- df %>%
    rename_with(~ new_colnames)
  
  return(df)
})

#remove month column
for (i in seq_along(month_split)){
  month_split[[i]] <- month_split[[i]] %>%
    ungroup() %>%
    select(-month)
}

#combine dataframes in list to make a single plotting frame
plot_df <- reduce(month_split, full_join, by = "direction")

#combine geospatial df with plot frame
colnames(mm_dat) <- str_replace_all(colnames(mm_dat), "^\\w{3}", str_to_title)
mm_dat <- mm_dat %>%
  rename(direction = Direction)
plot_df <- full_join(plot_df, mm_dat, by = "direction")

#======================================================
#=====================plotting=========================
#======================================================
create_plot <- function(month_abbr, month_full, variable) {
  variable_col <- paste0(month_abbr, "_", variable)
  canopy_cover_col <- paste0(month_abbr, "_CHM")
  
  ggplot(plot_df) +  
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(4, 5)), 
      color = "lightgrey"
    ) +
    geom_col(
      aes_string(
        x = "direction",
        y = 5,
        fill = variable_col  
      ),
      position = "dodge2"
    ) +
    geom_segment(
      aes_string(
        x = "direction",
        y = 5,
        xend = "direction", 
        yend = 5
      ),
      linetype = "dashed",
      color = "white"
    ) + 
    coord_polar() +
    scale_fill_viridis_c(option = "viridis", name = "") +
    theme(
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_blank(),  
      legend.position = "bottom",
      text = element_text(color = "gray12", family = "Bell MT"),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(title = month_full) +
    guides(
      fill = guide_colorbar(barwidth = 8, barheight = 0.5)
    )
}

#abbreviations for pasting onto variable names
#full month names for plot titles
month_abbr <- month.abb
month_full <- month.name

#define variable (didn't want to write a nested loop, so produce plots one at a time)
variable <- "CHM"

#create individual plots with loop
plots <- Map(create_plot, month_abbr, month_full, MoreArgs = list(variable = variable))

#combine plots into a single frame
final_plot <- wrap_plots(plots, ncol = 6) & 
  theme(legend.position = "bottom")
final_plot+ 
  plot_annotation(title = "Average Canopy Height")

#====================
#====================
#====================
#just plotting avg gpp; no z score
#columns separated by month and named accordingly so functions to make col names and loop through
create_plot <- function(month_abbr, month_full, variable) {
  col_name <- paste0(month_abbr, "_", variable)
  
  ggplot(plot_df) +  
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(0, 16)), 
      color = "lightgrey"
    ) +
    geom_col(
      aes_string(
        x = "direction",
        y = 16,
        fill = col_name
      ),
      position = "dodge2"
    ) +
    geom_segment(
      aes_string(
        x = "direction",
        y = 16,
        xend = "direction", 
        yend = 16
      ),
      linetype = "dashed",
      color = "gray12"
    ) + 
    coord_polar() +
    scale_fill_viridis_c(option = "viridis", name = "", breaks = pretty(plot_df[[col_name]], n = 5)) +
    theme(
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_blank(),  
      legend.position = "bottom",
      text = element_text(color = "gray12", family = "Bell MT"),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(title = month_full)+
    guides(
      fill = guide_colorbar(barwidth = 8, barheight = 0.5)
    )
}

# Abbreviations and full names for months
month_abbr <- month.abb
month_full <- month.name

# Define the variable suffix (e.g., 'gpp' in Jan_gpp, Feb_gpp, etc.)
variable <- "gpp"

# Create individual plots with loop
plots <- Map(create_plot, month_abbr, month_full, MoreArgs = list(variable = variable))

# Combine plots into a single frame
final_plot <- wrap_plots(plots, ncol = 6) & 
  theme(legend.position = "bottom")

final_plot + 
  plot_annotation(title = "Average Directional GPP")
  
