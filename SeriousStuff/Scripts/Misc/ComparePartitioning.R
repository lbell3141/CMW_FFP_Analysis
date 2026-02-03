#demonstrate partitioning is irrelevant 
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(viridis)
library(patchwork)
library(tibble)


csv_files <- list.files("./Data/RussHomework/GapfilledPartitionedFluxes_US-SRG_HH_ALL", pattern = "\\.csv$", full.names = TRUE)
fulldata <- do.call(rbind, lapply(csv_files, read.csv))
SRGtowerdat <- fulldata%>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))

SRGfluxdat <- read.csv("./SeriousStuff/Data/AMF_US-SRG_FLUXNET_FULLSET_HH_2008-2024_5-7.csv", na.strings = "-9999", skip = 0)%>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))

gpp_joined <- SRGtowerdat %>%
  dplyr::select(TIMESTAMP_END, GPP_tower = GPP) %>%
  dplyr::inner_join(
    SRGfluxdat %>%
      dplyr::select(TIMESTAMP_END, dplyr::starts_with("GPP")),
    by = "TIMESTAMP_END"
  )


# 2. Identify flux GPP columns
gpp_flux_cols <- names(gpp_joined) %>%
  str_subset("^GPP") %>%
  setdiff("GPP_tower")

gpp_r2_summary <- purrr::map_dfr(gpp_flux_cols, function(col) {
  
  dat <- gpp_joined %>%
    dplyr::select(GPP_tower, flux = dplyr::all_of(col)) %>%
    dplyr::filter(!is.na(GPP_tower), !is.na(flux))
  
  r2 <- if (nrow(dat) > 2) {
    stats::cor(dat$GPP_tower, dat$flux, use = "complete.obs")^2
  } else {
    NA_real_
  }
  
  tibble::tibble(
    flux_GPP_column = col,
    n = nrow(dat),
    R2 = r2
  )
})


# 4. Sort by performance
gpp_r2_summary <- gpp_r2_summary %>%
  mutate(R2 = as.numeric(R2))%>%
  arrange(desc(R2))
