#subsetting 10Hz data into month folders to run through arrow_test.R

setwd("E:/CM10Hz_text")

year_folders <- list.dirs(recursive = FALSE, full.names = TRUE)

#organize files within year folder into month folders
group_by_month <- function(year_folder) {

    files <- list.files(year_folder, full.names = TRUE)
  
  #extract month from name
  months <- sapply(files, function(file) {
    # Extract the MM part from the filename using regex
    month_num <- sub(".*_(\\d{4}_(\\d{2}))_\\d{2}_\\d{3}.*$", "\\2", basename(file))
    month_num
  })
  
  #create month folders (01 through 12)
  for (month in sprintf("%02d", 1:12)) {
    #month folder path
    month_folder <- file.path(year_folder, month)
    
    if (!dir.exists(month_folder)) {
      dir.create(month_folder)
    }
    #move files
    files_to_move <- files[months == month]
    for (file in files_to_move) {
      file.rename(file, file.path(month_folder, basename(file)))
    }
  }
}

lapply(year_folders, group_by_month)
