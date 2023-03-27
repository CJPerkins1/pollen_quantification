# Introduction ------------------------------------------------------------
# This script will make a set of image file locations so I can copy them all
# into the same directory to do inference. The images are those used for 
# validation. I've made a separate Python script that gets the total counts
# for each class from the validation tfrecord file. I'll import the file 
# with these counts first, because it also contains the file names I'll need.

library(tidyr)
library(dplyr)
library(lubridate)

get_validation_file_names <- function(ground_truth_file){
  df <- read.table(file = file.path(getwd(), "data", "ground_truth", ground_truth_file),
                   sep = '\t',
                   header = TRUE)
  df <- df %>%
    separate(name, c("date", "run", "temp_target", "well", "frame_num"), sep = '_')
  
  # Paths change when camera switches
  camera_switch_date <- ymd("2022-05-27")

  df <- df %>%
    mutate(path_string = if_else(ymd(date) <= camera_switch_date,
                                 paste0("/media/volume/sdb/jpgs/",
                                        date, "_", run, "_", temp_target, "_stab/well_", well, "/",
                                        date, "_", run, "_", temp_target, "_", well, "_", frame_num, "_stab.jpg"),
                                 paste0("/media/volume/sdb/norm_stab_jpgs/",
                                        date, "_", run, "_", temp_target, "_normalized_stabilized/well_", well, "/",
                                        date, "_", run, "_", temp_target, "_", well, "_", frame_num, "_stab.jpg")))
  
  paths <- unique(df$path_string)
  
  file_name_string <- substr(ground_truth_file, 0, nchar(ground_truth_file) -4)
  write.table(paths,
              file = file.path(getwd(), "data", "ground_truth", paste0(file_name_string, "_paths.txt")),
              row.names = F,
              col.names = F,
              quote = F)
}

get_validation_file_names("ground_truth_2022-12-12.tsv")
get_validation_file_names("ground_truth_2022-12-15.tsv")
get_validation_file_names("ground_truth_2022-12-16.tsv")

get_validation_file_names("ground_truth_2023-03-24.tsv")

# The bash command to copy them is:
# cat /home/exouser/scratch/val_file_names_2022-12-12.txt | xargs -I % cp % /media/volume/sdb/scratch/val_set_2022-12-12
# cat /home/exouser/scratch/val_file_names_2022-12-15.txt | xargs -I % cp % /media/volume/sdb/scratch/val_set_2022-12-15
# cat /home/exouser/scratch/val_file_names_2022-12-16.txt | xargs -I % cp % /media/volume/sdb/scratch/val_set_2022-12-16
# cat /home/exouser/scratch/val_file_names_2023-03-24.txt | xargs -I % cp % /media/volume/sdb/scratch/val_set_2022-03-24
