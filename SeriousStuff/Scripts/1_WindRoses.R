library(dplyr)
library(lubridate)
library(openair) #windrose-specific package

# func to read in data, format date, and pull wind data cols
read_wind <- function(file, ws_col, wd_col, skip = 0) {
  read.csv(file, na.strings = "-9999", skip = skip) %>%
    mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START))) %>%
    transmute(
      wind_sp  = as.numeric(.data[[ws_col]]),
      wind_dir = as.numeric(.data[[wd_col]])
    ) %>%
    filter(!is.na(wind_sp), !is.na(wind_dir))
}

# read in data from all 4 sites
cmw <- read_wind(
  "./SeriousStuff/Data/FluxData/PI_DATA/Full_PI_Data_by_Site/CMW_FullFluxes.csv",
  ws_col = "WS_1_1_1",
  wd_col = "WD_1_1_1",
  skip = 2
)

srg <- read_wind(
  "./SeriousStuff/Data/FluxData/PI_DATA/Full_PI_Data_by_Site/SRG_FullFluxes.csv",
  ws_col = "WS_1_1_1",
  wd_col = "WD_1_1_1"
)

srm <- read_wind(
  "./SeriousStuff/Data/FluxData/PI_DATA/Full_PI_Data_by_Site/SRM_FullFluxes.csv",
  ws_col = "WS_1_1_1",
  wd_col = "WD_1_1_1"
)

wkg <- read_wind(
  "./SeriousStuff/Data/FluxData/PI_DATA/Full_PI_Data_by_Site/WKG_FullFluxes.csv",
  ws_col = "WS_1_1_1",
  wd_col = "WD_1_1_1"
)

#combine frames together
all_wind <- bind_rows(cmw, srg, srm, wkg)

max_ws <- ceiling(max(all_wind$wind_sp, na.rm = TRUE))

# Define shared bins
ws_breaks <- c(0, 2, 4, 6, 8, 10, max_ws)
n_cols <- length(ws_breaks) - 1


plot_windrose <- function(data, site, outfile, breaks) {
  png(
    filename = outfile,
    width = 1800,
    height = 1800,
    res = 300,
    bg = "white"
  )
  
  par(bg = NA)
  
  windRose(
    mydata = data,
    ws = "wind_sp",
    wd = "wind_dir",
    
    angle = 20,
    type = "default",
    paddle = FALSE,
    width = 1.1,
    
    breaks = breaks,
    cols = viridis(n_cols, option = "D", direction = 1),
    
    auto.text = TRUE,
    fontsize = 15,
    col = "white",
    
    key.footer = "",
    key.width = 3,
    calm = 0,
    
    main = site
  )
  
  dev.off()
}


plot_windrose(cmw, "CMW", "./SeriousStuff/Figures/Windroses/CMW_windrose_daytime.png", ws_breaks)
plot_windrose(srg, "SRG", "./SeriousStuff/Figures/Windroses/SRG_windrose_daytime.png", ws_breaks)
plot_windrose(srm, "SRM", "./SeriousStuff/Figures/Windroses/SRM_windrose_daytime.png", ws_breaks)
plot_windrose(wkg, "Wkg", "./SeriousStuff/Figures/Windroses/Wkg_windrose_daytime.png", ws_breaks)
