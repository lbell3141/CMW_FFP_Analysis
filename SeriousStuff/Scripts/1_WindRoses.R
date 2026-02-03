library(dplyr)
library(lubridate)
library(openair)



#CMW============================================================================
cmwdat_file <- read.csv(
  "./SeriousStuff/Data/AMF_US-CMW_BASE_HH_2-5.csv",
  na.strings = "-9999",
  skip = 2
) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

cmwdat_voi <- cmwdat_file %>%
  transmute(
    wind_sp  = WS_1_1_1,
    wind_dir = WD_1_1_1
  ) %>%
  filter(
    !is.na(wind_sp),
    !is.na(wind_dir)
  )

png(
  filename = "CMW_windrose_daytime.png",
  width = 1800,
  height = 1800,
  res = 300,
  bg = "transparent"
)

par(bg = NA)


windRose(
  mydata = cmwdat_voi,
  ws = "wind_sp",
  wd = "wind_dir",
  
  angle = 20,
  type = "default",
  paddle = F,
  width = 1.1,
  
  cols = "jet",
  
  auto.text = TRUE,
  fontsize = 15,
  col = "white",
  
  key.footer = "",
  calm = 0,
  
  main = "CMW"
)
dev.off()

#SRG============================================================================
srgdat_file <- read.csv(
  "./SeriousStuff/Data/AMF_US-SRG_FLUXNET_FULLSET_HH_2008-2024_5-7.csv",
  na.strings = "-9999"
) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

srgdat_voi <- srgdat_file %>%
  transmute(
    wind_sp  = WS,
    wind_dir = WD
  ) %>%
  filter(
    !is.na(wind_sp),
    !is.na(wind_dir)
  )

png(
  filename = "SRG_windrose_daytime.png",
  width = 1800,
  height = 1800,
  res = 300,
  bg = "transparent"
)

par(bg = NA)


windRose(
  mydata = srgdat_voi,
  ws = "wind_sp",
  wd = "wind_dir",
  
  angle = 20,
  type = "default",
  paddle = F,
  width = 1.1,
  
  cols = "jet",
  
  auto.text = TRUE,
  fontsize = 15,
  col = "white",
  
  key.footer = "",
  calm = 0,
  
  main = "SRG"
)
dev.off()

#SRM============================================================================
srmdat_file <- read.csv(
  "./SeriousStuff/Data/AMF_US-SRM_FLUXNET_FULLSET_HH_2004-2024_4-7.csv",
  na.strings = "-9999"
) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

srmdat_voi <- srmdat_file %>%
  transmute(
    wind_sp  = WS,
    wind_dir = WD
  ) %>%
  filter(
    !is.na(wind_sp),
    !is.na(wind_dir)
  )

png(
  filename = "SRM_windrose_daytime.png",
  width = 1800,
  height = 1800,
  res = 300,
  bg = "transparent"
)

par(bg = NA)


windRose(
  mydata = srmdat_voi,
  ws = "wind_sp",
  wd = "wind_dir",
  
  angle = 20,
  type = "default",
  paddle = F,
  width = 1.1,
  
  cols = "jet",
  
  auto.text = TRUE,
  fontsize = 15,
  col = "white",
  
  key.footer = "",
  calm = 0,
  
  main = "SRM"
)
dev.off()

#Wkg============================================================================
wkgdat_file <- read.csv(
  "./SeriousStuff/Data/AMF_US-Wkg_FLUXNET_FULLSET_HH_2004-2024_4-7.csv",
  na.strings = "-9999"
) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

wkgdat_voi <- wkgdat_file %>%
  transmute(
    wind_sp  = WS,
    wind_dir = WD
  ) %>%
  filter(
    !is.na(wind_sp),
    !is.na(wind_dir)
  )

png(
  filename = "Wkg_windrose_daytime.png",
  width = 1800,
  height = 1800,
  res = 300,
  bg = "transparent"
)

par(bg = NA)


windRose(
  mydata = wkgdat_voi,
  ws = "wind_sp",
  wd = "wind_dir",
  
  angle = 20,
  type = "default",
  paddle = F,
  width = 1.1,
  
  cols = "jet",
  
  auto.text = TRUE,
  fontsize = 15,
  col = "white",
  
  key.footer = "",
  calm = 0,
  
  main = "Wkg"
)
dev.off()








