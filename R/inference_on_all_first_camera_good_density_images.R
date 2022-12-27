# Introduction ------------------------------------------------------------
# I'll do the inference on the first batch of images here. I want to try to 
# come up with some good visualizations of the data for PAG first and 
# probably won't do much actual quantification until later. I will use 
# Arrow because the file is huge and I've been wanting to try it out.

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)


# Importing the data ------------------------------------------------------
# Using Arrow to import the data as an Arrow Table. 
inference <- read_delim_arrow(file = file.path(getwd(), 
                                               "data", 
                                               "full_inference", 
                                               "test_inference.txt"),
                              delim = '\t',
                              col_names = TRUE,
                              as_data_frame = FALSE)
# Probably doing a schema is better than this, but at least doing while 
# importing has some weird error with not properly skipping the first line 
# and getting confused with the column titles.
inference$score <- as.double(inference$score)

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
    mutate(percentage = replace_na(percentage, 0))
    # collect() # or compute() to return another Arrow Table
  
  return(df)
}

processed_inference <- process_data(inference, 
                                    0.39, # germinated_cutoff
                                    0.47, # ungerminated_cutoff 
                                    0.59, # burst_cutoff
                                    0.2, # unknown_germinated_cutoff
                                    0.45) # aborted_cutoff

# Making a plot -----------------------------------------------------------

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
  
  ggplot(input_df, aes(x = timepoint, y = percentage, color = class)) +
    # geom_line(size = 2) +
    geom_smooth(span = 0.4, se = FALSE, size = 2) +
    geom_point(size = 2) +
    scale_color_manual(values = color_vec) +
    scale_y_continuous(breaks = c(0, 0.25, .5, .75, 1), 
                       labels = c("0%", "25%", "50%", "75%", "100%"),
                       limits = c(0, 1)) +
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
  
  ggsave(filename = file.path(getwd(), "plots", paste0(image_name, ".png")),
         device = 'png',
         width = 12,
         height = 8,
         dpi = 400,
         units = 'in')
}

make_plot(output_df, name)

