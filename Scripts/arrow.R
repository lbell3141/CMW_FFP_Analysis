library(arrow)
library(tictoc)
library(dplyr)
library(duckdb)

tic()
#opening dataset as a tab sep file with source location
hf_dataset <-
  arrow::open_tsv_dataset(
    sources = here::here(
      "Data/High_Freq_Data",
      "Jan2019.txt"))
#making schema object and formatting TimeStamp explicitly as a character string
hf_dataset_schema <- schema(hf_dataset)
hf_dataset_schema$TimeStamp <- string()
#reopening the dataset and applying modified schema
hf_dataset <-
  arrow::open_tsv_dataset(
    sources = here::here(
      "Data/High_Freq_Data",
      "Jan2019.txt"), 
    schema = hf_dataset_schema,
    skip = 1
  )

#pass to duckdb and add row ID column and group ID 
hf_dataset <-  hf_dataset|>
  to_duckdb() |>
  mutate(row_ID = row_number()) |>
  mutate(group_ID = (row_ID - 1) %/% 30) 

#create df with time for each group
#use arrange or output will be in random order, even though calculated correctly
group_date <- hf_dataset |>
  group_by(group_ID) |>
  summarize(TimeStamp = first(TimeStamp)) |>
  arrange(group_ID) 

#average high freq data into 3s using groups
#then, calculate wind speed and direction from 3s vector components
ag3_dataset <- hf_dataset |>
  group_by(group_ID) |>
  summarise(
    Time = max(TimeStamp, na.rm = TRUE),
    Ux = mean(Ux, na.rm = TRUE),
    Uy = mean(Uy, na.rm = TRUE),
    Uz = mean(uZ, na.rm = TRUE))|>
  mutate(
    U_i = ((((Ux)^2) + ((Uy)^2)))^(1/2),
    theta_stand = atan2(Ux, Uy)) |>
  arrange(group_ID)

#function to convert to compass degrees from standard plane radians
radians_to_compass <- function(angle_rad) {
  angle_deg <- angle_rad * (180 / pi)  
  compass_angle <- (90 - angle_deg) %% 360  
  return(compass_angle)
}

#apply function (convert rads to degs)
ag3_dataset <- ag3_dataset |>
  mutate(theta_cc = radians_to_compass(theta_stand))

#create groups for half hours
bar_df_HH <- ag3_dataset |>
  #to_duckdb() |>
  mutate(HH_group_ID = (group_ID - 1) %/% 600)

#calculate half hourly wind speed (u_i_bar) and direction (theta_v_bar)
#calculate half hourly st dev of lateral wind velocity (sigma_v)

bar_df_HH <- bar_df_HH |>
  group_by(HH_group_ID) |>
  summarise(
    u_i_bar = (1 / n()) * sum(U_i),
    u_x_bar = (1 / n()) * sum(U_i * sin(theta_stand - pi)),
    u_y_bar = (1 / n()) * sum(U_i * cos(theta_stand - pi)))|>
  summarise(
    theta_v_bar = atan2(u_x_bar, u_y_bar) + pi,
    sigma_v_sqd = (1 / (n() - 1)) * sum((U_i * sin(theta_v_bar - theta_stand))^2),
    sigma_v = sqrt(abs(sigma_v_sqd)))|>
  to_arrow()

#select variables of interest to write to csv
voi_dataset <- bar_df_HH |>
  select(TimeStamp, u_i_bar, theta_v_bar, sigma_v)

#bind final product to Ameriflux CMW record
#to_arrow > write_csv_arrow
write_csv_arrow(
  voi_dataset,
  "./Data/High_Freq_Data/Processed/output_A.csv",
  col_names = T)

toc()

