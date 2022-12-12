# Introduction ------------------------------------------------------------
# This script will make a set of image file locations so I can copy them all
# into the same directory to do inference. The images are those used for 
# validation. I've made a separate Python script that gets the total counts
# for each class from the validation tfrecord file. I'll import the file 
# with these counts first, because it also contains the file names I'll need.

library(tidyr)

df <- read.table(file = file.path(getwd(), "data", "ground_truth_2022-12-12.tsv"),
                 sep = '\t',
                 header = TRUE)
df <- df %>%
  separate(name, c("date", "run", "temp_target", "well", "frame_num"), sep = '_')

base_dir_path <- "/media/volume/sdb/jpgs/"

df$path_string <- paste0(base_dir_path,
                            df$date,
                            "_",
                            df$run,
                            "_",
                            df$temp,
                            "_stab/well_",
                            df$well,
                            "/",
                            df$date,
                            "_",
                            df$run,
                            "_",
                            df$temp_target,
                            "_",
                            df$well,
                            "_",
                            df$frame_num,
                            "_stab.jpg")

paths <- unique(df$path_string)

write.table(paths,
            file = file.path(getwd(), "data", "ground_truth_paths.txt"),
            row.names = F,
            col.names = F,
            quote = F)

# The bash command to copy them is:
# cat /home/exouser/scratch/val_file_names_2022-12-12.txt | xargs -I % cp % /media/volume/sdb/scratch/val_set_2022-12-12