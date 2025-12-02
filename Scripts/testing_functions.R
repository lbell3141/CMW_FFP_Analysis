# SRG Plots

HH_dat <- Load_FluxData("US-SRM")

plot_monthly_metmodelresiduals(mm_results)

dat_avgd <- calc_monthly_AvgDirectionFlux(HH_dat)

plot_carbonflux_heatmaps(dat_avgd)

plot_monthly_directional_gpp(avgdat)

lidar_input_path <- "Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las"
lai_output_path <- "./testlairast.tif"


lairasteroutputfromfunction <- lai_from_lidar("Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las", "./testlairast.tif")
