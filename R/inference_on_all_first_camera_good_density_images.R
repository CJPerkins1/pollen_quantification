# Introduction ------------------------------------------------------------
# I'll do the inference on the first batch of images here. I want to try to 
# come up with some good visualizations of the data for PAG first and 
# probably won't do much actual quantification until later. I will use 
# Arrow because the file is huge and I've been wanting to try it out.

library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(googlesheets4)

# Adding my Google service account credentials
gs4_auth(path = "~/.credentials/google_sheets_api/service_account.json")

# Importing the data ------------------------------------------------------
# Using Arrow to import the data as an Arrow Table. 
inference <- read_delim_arrow(file = file.path(getwd(), 
                                               "data", 
                                               "full_inference", 
                                               "inference_2022-12-26.tsv"),
                              delim = '\t',
                              col_names = TRUE,
                              as_data_frame = FALSE)
# Probably doing a schema is better than this, but at least doing while 
# importing has some weird error with not properly skipping the first line 
# and getting confused with the column titles.
inference$score <- as.double(inference$score)

# None of the images are named after their accession, just the wells. The
# required information to link well to accession can be found in this sheet:
wells_to_accessions <- read_sheet("1yQ5yAKiL6BzwZ-wH-Q44RoUEwMZztTYafzdvVylq6fo")

# Only keeping the columns we need
wells_to_accessions <- wells_to_accessions[ , c("date", "run", "well", "temp_target", "accession")]
wells_to_accessions$date <- as.character(wells_to_accessions$date)


# Cleaning up the data ----------------------------------------------------
# Clean-up, confidence cutoffs, adding accession info
process_data <- function(df, 
                         germinated_cutoff, 
                         ungerminated_cutoff, 
                         burst_cutoff, 
                         unknown_germinated_cutoff, 
                         aborted_cutoff){
  df <- df %>%
    filter(case_when(class == "germinated" ~ score >= {{germinated_cutoff}},
                     class == "ungerminated" ~ score >= {{ungerminated_cutoff}},
                     class == "burst" ~ score >= {{burst_cutoff}},
                     class == "unknown_germinated" ~ score >= {{unknown_germinated_cutoff}},
                     class == "aborted" ~ score >= {{aborted_cutoff}})) %>%
    mutate(name = paste0(
      date,
      "_run",
      run,
      "_",
      tempc,
      "C_",
      well,
      "_t",
      str_pad(timepoint, 3, pad = "0")
    )) %>%
    group_by(name, class) %>%
    summarize(count = n()) %>%
    collect %>%
    mutate(percentage = count / sum(count)) %>%
    ungroup() %>%
    complete(name, class) %>%
    mutate(count = replace_na(count, 0)) %>%
    mutate(percentage = replace_na(percentage, 0)) %>%
    mutate(time = as.integer(str_sub(name, -3, -1)),
           date = str_sub(name, 1, 10),
           run = as.double(str_sub(name, 15, 15)),
           well = str_sub(name, 21, 22)) %>%
    filter(time <= 82)
    # collect() # or compute() to return another Arrow Table
  
  return(df)
}

processed_inference <- process_data(inference, 
                                    0.39, # germinated_cutoff
                                    0.47, # ungerminated_cutoff 
                                    0.59, # burst_cutoff
                                    0.2, # unknown_germinated_cutoff
                                    0.45) # aborted_cutoff

# Adding back in the accession and temp metadata
processed_inference <- left_join(processed_inference, wells_to_accessions, by = c("date", "run", "well"))

# Testing the results
test_df <- tail(processed_inference, 10000)


# Making a simplified data frame for plotting -----------------------------
# I'll try just taking the averages across accessions first, just to have a 
# chance of even getting everything on the plot.
simplified_df <- processed_inference %>%
  group_by(accession, temp_target, time, class) %>%
  summarize(mean_percentage = mean(percentage))


# Making plots with the simplified data -----------------------------------
make_plot <- function(input_df, image_name) {
  color_vec <- c("#DC267F", # burst
                 "#5fc77b", # germinated
                 "#2F69FF", # ungerminated
                 "#FFB000", # unknown_germinated
                 "#787878", # aborted
                 "#ffa6db", # tube_tip_burst
                 "#fffa70", # tube_tip_bulging
                 "#a8ffe1") # tube_tip
  names(color_vec) <- c("burst", 
                        "germinated", 
                        "ungerminated", 
                        "unknown_germinated", 
                        "aborted", 
                        "tube_tip_burst",
                        "tube_tip_bulging",
                        "tube_tip")
  
  ggplot(input_df, aes(x = time, y = mean_percentage, color = class, linetype = accession)) +
    geom_line(linewidth = 0.5, alpha = 0.5) +
    # geom_smooth(span = 0.4, se = FALSE, size = 2) +
    # geom_point(size = 2) +
    scale_color_manual(values = color_vec) +
    # scale_linetype_manual(values = rep.int(1, 186)) +
    scale_linetype_manual(values = rep.int(1, 191)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(breaks = c(0, 0.25, .5, .75, 1),
                       labels = c("0%", "25%", "50%", "75%", "100%"),
                       limits = c(0, 1),
                       expand = c(0, 0)) +
    labs(title = image_name,
         y = "Percentage") +
    theme_bw() +
    theme(axis.title = element_text(size = 26, face = 'bold'),
          axis.text = element_text(size = 22, face = 'bold', color = 'black'),
          axis.text.x = element_text(size = 26, face = 'bold', color = 'black'),
          plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
          axis.title.x = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(size = 1, color = 'black'),
          axis.ticks = element_line(size = 1, color = 'black'),
          axis.ticks.length = unit(8, 'pt'),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
          panel.grid = element_blank(),
          legend.position = 'none',
          strip.background = element_blank(),
          strip.placement = "outside")
  
  ggsave(filename = file.path(getwd(), "plots", "all_inference_plots", paste0(image_name, ".png")),
         device = 'png',
         width = 12,
         height = 8,
         dpi = 400,
         units = 'in')
}

make_plot(simplified_df[simplified_df$temp_target == 26, ], "All inference from first camera at 26C")
make_plot(simplified_df[simplified_df$temp_target == 34, ], "All inference from first camera at 34C")

