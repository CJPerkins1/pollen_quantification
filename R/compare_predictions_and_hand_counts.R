#hand_count Introduction ------------------------------------------------------------
# I'll use this script to compare predictions from the model and hand counts.
# I'll also look at how the percent confidence cutoff affects accuracy. 

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lemon) # For facet axes


# Importing the data ------------------------------------------------------
## Ground truth
ground_truth_2022_12_12 <- read.table(file = file.path(getwd(), "data", "ground_truth", "ground_truth_2022-12-12.tsv"),
                           sep = '\t',
                           header = TRUE)

ground_truth_2022_12_15 <- read.table(file = file.path(getwd(), "data", "ground_truth", "ground_truth_2022-12-15.tsv"),
                           sep = '\t',
                           header = TRUE)

## Inference
# All classes
resnet_2022_12_12 <- read.table(file = file.path(getwd(), "data", "validation_inference", "2022-12-12_validation_image_predictions.tsv"),
                                sep = '\t',
                                header = TRUE)

# Just pollen classes, but also nms
centernet_2022_12_14 <- read.table(file = file.path(getwd(), "data", "validation_inference", "2022-12-14_centernet_val_only_pollen_w_nms_predictions.tsv"),
                                sep = '\t',
                                header = TRUE)

centernet_2022_12_15 <- read.table(file = file.path(getwd(), "data", "validation_inference", "2022-12-15_centernet_val_predictions.tsv"),
                                   sep = '\t',
                                   header = TRUE)


# Getting r-squared values ------------------------------------------------
get_r_squared <- function(ground_truth, inference) {
  # Processing the ground truth
  process_ground_truth <- function(df){
    df <- df %>%
      complete(name, class) %>%
      mutate(hand_count = replace_na(size, 0)) %>%
      select(-size)
    return(df)
  }
  ground_truth <- process_ground_truth(ground_truth)
  
  # Sub function that calculates the R-squared for a single threshold and a single class
  get_single_r_squared <- function(ground_truth, inference, threshold, class_string) {
    # Processing the inference with confidence score cutoff (threshold)
    process_inference <- function(df, confidence_cutoff){
      df <- df %>%
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
        filter(score >= confidence_cutoff) %>%
        group_by(name, class) %>%
        summarize(model_count = n(), .groups = "drop") %>%
        complete(name, class) %>%
        mutate(model_count = replace_na(model_count, 0)) 
      
      return(df)
    }
    
    inference <- process_inference(inference, threshold)
    
    # Combine the two data frames
    df <- full_join(ground_truth, inference, by = c("name", "class"))
    
    df <- df %>%
      filter(class == class_string) %>%
      ungroup() %>%
      complete(name, class) %>%
      mutate(model_count = replace_na(model_count, 0))
    
    # Doing the regression
    regression_r <- summary(lm(hand_count ~ model_count, data = df))$adj.r.squared
    output <- data.frame("class" = class_string, 
                         "threshold" = threshold, 
                         "r_squared" = regression_r)
    return(output)
    
    # # Returns 0 when there are no bounding boxes that satisfy a threshold
    # if (nrow(df) == 0) {
    #   output <- data.frame("threshold" = threshold, "r_squared" = 0)
    #   return(output)
    # 
    #   # Runs the regression if bounding boxes are present
    # } else {
    #   regression_r <- summary(lm(count ~ GFP_hand, data = df))$adj.r.squared
    #   output <- data.frame("threshold" = threshold, "r_squared" = regression_r)
    #   return(output)
    # }
  }
  
  # Going through each class and calculating the r-squared values at intervals of 0.01
  output_df <- data.frame()
  
  for (class_string in unique(ground_truth$class)) {
    df_list <- list()
    i = 1
    for (x in seq(0.01, 1, by=0.01)) {
      output <- get_single_r_squared(ground_truth, inference, x, class_string)
      df_list[[i]] <- output
      i = i + 1
    }
    r_squared_df <- bind_rows(df_list)
    output_df <- rbind(output_df, r_squared_df)
  }
  
  output_df <- output_df %>%
    filter(r_squared != 0)
  
  # Print the max R-squared values for each class
  print("Max R-squared values:")
  for (class_string in unique(output_df$class)) {
    print_df <- output_df %>%
      filter(class == class_string)
    r_squared_val = as.character(max(print_df$r_squared))
    threshold_at_max_r_squared = as.character(print_df$threshold[which.max(print_df$r_squared)])
    print(paste0(class_string, ": ", r_squared_val, " at threshold ", threshold_at_max_r_squared))
  }
  
  return(output_df)
}

rsq_resnet_2022_12_12 <- get_r_squared(ground_truth_2022_12_12, resnet_2022_12_12)
rsq_centernet_2022_12_14 <- get_r_squared(ground_truth_2022_12_12, centernet_2022_12_14)
rsq_centernet_2022_12_15 <- get_r_squared(ground_truth_2022_12_15, centernet_2022_12_15)


# Plotting r-squared values -----------------------------------------------
plot_r_squared <- function(df, model_name) {
  color_vec <- c("#DC267F", # burst
                 "#5fc77b", # germinated
                 "#2F69FF", # ungerminated
                 "#FFB000", # unknown_germinated
                 "#787878",   # aborted
                 "#ffa6db", # tube_tip_burst
                 "#fffa70", # tube_tip_bulging
                 "#a8ffe1") #tube_tip
  names(color_vec) <- c("burst", 
                        "germinated", 
                        "ungerminated", 
                        "unknown_germinated", 
                        "aborted", 
                        "tube_tip_burst",
                        "tube_tip_bulging",
                        "tube_tip")
  
  ggplot(df, aes(x = threshold, y = r_squared, color = class)) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), limits = c(0, 1)) +
    scale_color_manual(values = color_vec,
                       name = "Class",
                       breaks = c("germinated", 
                                  "ungerminated", 
                                  "burst", 
                                  "aborted", 
                                  "unknown_germinated", 
                                  "tube_tip", 
                                  "tube_tip_burst", 
                                  "tube_tip_bulging"),
                       labels = c("Germinated", 
                                  "Ungerminated", 
                                  "Burst", 
                                  "Aborted", 
                                  "Unknown germinated", 
                                  "Tube tip", 
                                  "Tube tip burst", 
                                  "Tube tip bulging"),
                       limits = force) +
    theme_bw() +
    labs(title = model_name, x = "Confidence threshold", y = "R-squared") +
    theme(axis.title = element_text(size = 26, face = 'bold'),
          axis.text = element_text(size = 22, face = 'bold', color = 'black'),
          axis.text.x = element_text(size = 26, face = 'bold', color = 'black'),
          plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
          panel.border = element_blank(),
          axis.line = element_line(size = 1, color = 'black'),
          axis.ticks = element_line(size = 1, color = 'black'),
          axis.ticks.length = unit(8, 'pt'),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside",
          legend.title = element_text(size = 18, face = 'bold', color = 'black'),
          legend.text = element_text(size = 14, face = 'bold', color = 'black'))
  
  ggsave(filename = file.path(getwd(), "plots", "r_squared", paste0(gsub(" ", "_", model_name), "_r_squared.png")),
         device = 'png',
         width = 12,
         height = 8,
         dpi = 400,
         units = 'in')
}

plot_r_squared(rsq_resnet_2022_12_12, "Faster RCNN Resnet 2022-12-12")
plot_r_squared(rsq_centernet_2022_12_14, "CenterNet HourGlass only pollen 2022-12-14")
plot_r_squared(rsq_centernet_2022_12_15, "CenterNet HourGlass all classes 2022-12-15")


# Plotting linear models --------------------------------------------------
# I want these plots to have the r-squared values on the plots. Probably 
# faceted.
make_and_plot_lm <- function(ground_truth, inference, confidence_threshold, model_name) {
  # Processing the ground truth
  process_ground_truth <- function(df){
    df <- df %>%
      complete(name, class) %>%
      mutate(hand_count = replace_na(size, 0)) %>%
      select(-size)
    return(df)
  }
  ground_truth <- process_ground_truth(ground_truth)
  
  # Processing the inference
  process_inference <- function(df, confidence_threshold){
    df <- df %>%
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
      filter(score >= confidence_threshold) %>%
      group_by(name, class) %>%
      summarize(model_count = n(), .groups = "drop") %>%
      complete(name, class) %>%
      mutate(model_count = replace_na(model_count, 0)) 
    
    return(df)
  }
  
  inference <- process_inference(inference, confidence_threshold)
  
  # Combining the two data frames
  df <- full_join(ground_truth, inference, by = c("name", "class"))
  
  df <- df %>%
    ungroup() %>%
    complete(name, class) %>%
    mutate(model_count = replace_na(model_count, 0)) %>%
    filter(model_count != 0)
  
  # Plotting
  color_vec <- c("#DC267F", # burst
                 "#5fc77b", # germinated
                 "#2F69FF", # ungerminated
                 "#FFB000", # unknown_germinated
                 "#787878",   # aborted
                 "#ffa6db", # tube_tip_burst
                 "#fffa70", # tube_tip_bulging
                 "#a8ffe1") #tube_tip
  names(color_vec) <- c("burst", 
                        "germinated", 
                        "ungerminated", 
                        "unknown_germinated", 
                        "aborted", 
                        "tube_tip_burst",
                        "tube_tip_bulging",
                        "tube_tip")
  
  ggplot(df, aes(x = hand_count, y = model_count, fill = class)) +
    geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = 2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") +
    geom_point(shape = 21, color = "black", size = 2) +
    # scale_x_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), limits = c(0, 1)) +
    scale_fill_manual(values = color_vec,
                       name = "Class",
                       breaks = c("germinated", 
                                  "ungerminated", 
                                  "burst", 
                                  "aborted", 
                                  "unknown_germinated", 
                                  "tube_tip", 
                                  "tube_tip_burst", 
                                  "tube_tip_bulging"),
                       labels = c("Germinated", 
                                  "Ungerminated", 
                                  "Burst", 
                                  "Aborted", 
                                  "Unknown germinated", 
                                  "Tube tip", 
                                  "Tube tip burst", 
                                  "Tube tip bulging"),
                       limits = force) +
    facet_rep_wrap(~class) +
    coord_fixed() +
    theme_bw() +
    labs(title = model_name, x = "Hand counts", y = "Model predictions") +
    theme(axis.title = element_text(size = 20, face = 'bold'),
          axis.text = element_text(size = 14, face = 'bold', color = 'black'),
          plot.title = element_text(size = 22, face = 'bold', margin = margin(0, 0, 10, 0)),
          panel.border = element_blank(),
          axis.line = element_line(size = 1, color = 'black'),
          axis.ticks = element_line(size = 1, color = 'black'),
          axis.ticks.length = unit(8, 'pt'),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(size = 12, face = 'bold', color = 'black'),
          legend.position = "none")
  
  ggsave(filename = file.path(getwd(), "plots", "linear_model", paste0(gsub(" ", "_", model_name), "_linear_model.png")),
         device = 'png',
         width = 12,
         height = 8,
         dpi = 400,
         units = 'in')
  

}

make_and_plot_lm(ground_truth_2022_12_12, resnet_2022_12_12, 0.05, "Faster RCNN Resnet 2022-12-12")
make_and_plot_lm(ground_truth_2022_12_12, centernet_2022_12_14, 0.3, "CenterNet HourGlass only pollen 2022-12-14")
make_and_plot_lm(ground_truth_2022_12_15, centernet_2022_12_15, 0.3, "CenterNet HourGlass all classes 2022-12-15")
