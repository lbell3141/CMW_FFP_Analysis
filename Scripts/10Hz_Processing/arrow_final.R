library(arrow)
library(tictoc)
library(dplyr)
library(duckdb)

#test_path <- here::here("E:/CM10Hz_text/", "TOA5_2003_01_01_000.dat")

#tic()...toc() reports processing time and is not integral to the actual processing code
tic()

#opening dataset as a tab sep file with source location; formatting error fixed below (15:26)
hf_dataset <- 
  arrow::open_dataset(
  "E:/CM10Hz_text/2021",
  #partitioning = "month",
  delim = ",",
  skip = 4,
  col_names = c("TIMESTAMP", "Ux", "Uy", "Uz", "co2", "h2o", "Ts", "press", "diag_csat"))|>
  mutate(year = 2021)
  

#making schema object and formatting TimeStamp explicitly as a character string
hf_dataset_schema <- schema(hf_dataset)
hf_dataset_schema$TIMESTAMP <- string()
#reopening the dataset and applying modified schema
hf_dataset <- 
  arrow::open_dataset(
    "E:/CM10Hz_text/2021",
    #partitioning = c("year", "month", "day"),
    delim = ",",
    skip = 4,
    col_names = c("TIMESTAMP", "Ux", "Uy", "Uz", "co2", "h2o", "Ts", "press", "diag_csat"))|>
  mutate(year = 2021)

#pass to duckdb and add row ID column and group ID 
hf_dataset <-  hf_dataset|>
  to_duckdb() |>
  mutate(row_ID = row_number()) |>
  mutate(group_ID = (row_ID - 1) %/% 30) 


#create df with time for each group
#use arrange or output will be in random order, even though calculated correctly
group_date <- hf_dataset |>
  group_by(group_ID) |>
  summarize(TIMESTAMP = first(TIMESTAMP)) |>
  arrange(group_ID) |>
  to_arrow()

#average high freq data into 3s using groups
#then, calculate wind speed and direction from 3s vector components
ag3_dataset <- hf_dataset |>
  group_by(group_ID) |>
  summarise(
    Time = max(TIMESTAMP, na.rm = TRUE),
    Year = max(year),
    Ux = mean(Ux, na.rm = TRUE),
    Uy = mean(Uy, na.rm = TRUE),
    Uz = mean(Uz, na.rm = TRUE))|>
  mutate(
    U_i = ((((Ux)^2) + ((Uy)^2)))^(1/2),
    theta_stand = atan2(Ux, Uy))

#function to convert to compass degrees from standard plane radians
#mutate instead of function
#ag3_dataset <- ag3_dataset |>
#  mutate(
#    angle_deg = theta_stand * (180 / pi),
#    comp_angle = (90 - angle_deg) %% 360 
#  ) 

#create groups for half hours
bar_df_HH <- ag3_dataset |>
  mutate(HH_group_ID = (group_ID) %/% 600) 

#calculate half hourly wind speed (u_i_bar) and direction (theta_v_bar)
#error using nested mutate/summarize, so calc averages and internal expressions before final sigma_v calculations
bar_df_HH <- bar_df_HH |>
  group_by(HH_group_ID) |>
  mutate(
    u_x_bar = (1 / n()) * sum(U_i * sin(theta_stand - pi)),
    u_y_bar = (1 / n()) * sum(U_i * cos(theta_stand - pi)),
    u_i_bar = (1 / n()) * sum(U_i),
    theta_v_bar = (atan2(u_x_bar, u_y_bar) + pi), 
    inside_sum = (U_i * sin((atan2(u_x_bar, u_y_bar) + pi) - theta_stand))^2
  )
#with defined terms, now calc sigmas                           
bar_df_HH <- bar_df_HH |>
  group_by(HH_group_ID) |>
  mutate(
    sigma_v_sqd = (1 / (n() - 1)) * sum(inside_sum),
    sigma_v = sqrt(abs(sigma_v_sqd))
  )|>
  ungroup()|>
  arrange(group_ID)

#select variables of interest in HH form
voi_dataset <- bar_df_HH |>
  group_by(HH_group_ID) |>
  summarize(
    HH_group_ID = HH_group_ID,
    Time = max(Time),
    Year = max(Year),
    u_i_bar = mode(u_i_bar),
    theta_v_bar = mode(theta_v_bar),
    sigma_v = mode(sigma_v)
  ) |>
  arrange(HH_group_ID)|>
  to_arrow()

#write to csv
  voi_dataset |>
  group_by(Year)|>
  write_dataset(
    "E:/CM10Hz_Processed",
    col_names = T,
    format = "csv")

toc()
