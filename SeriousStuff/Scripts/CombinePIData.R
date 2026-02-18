#Stitch together annual and multiannual records of tower obs
#Output single csv flux obs for each site

library(readr)
library(dplyr)

#CMW _PI fluxes already available in base product; no need to stitch files

#SRG
in_dir <- "./SeriousStuff/Data/FluxData/PI_DATA/GapfilledPartitionedFluxes_US-SRG_HH_ALL"
out_dir <- "./SeriousStuff/Data/FluxData/PI_DATA/SRG_FullFluxes.csv"

dat_files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)

stitched_df <- dat_files %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

#write_csv(stitched_df, out_dir)

#SRM
in_dir <- "./SeriousStuff/Data/FluxData/PI_DATA/SRM_2003_2017"
out_dir <- "./SeriousStuff/Data/FluxData/PI_DATA/SRM_FullFluxes.csv"

dat_files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)

stitched_df <- dat_files %>%
  lapply(read_csv, na = "-9999", show_col_types = FALSE) %>%
  bind_rows()

#write_csv(stitched_df, out_dir)

#WKG
in_dir <- "./SeriousStuff/Data/FluxData/PI_DATA/WKG_2004_2024"
out_dir <- "./SeriousStuff/Data/FluxData/PI_DATA/WKG_FullFluxes.csv"

dat_files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)

stitched_df <- dat_files %>%
  lapply(read_csv, na = "-9999", show_col_types = FALSE) %>%
  bind_rows()

#write_csv(stitched_df, out_dir)


