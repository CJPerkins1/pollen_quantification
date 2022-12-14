# Introduction ------------------------------------------------------------
# I'll use this script to compare predictions from the model and hand counts.
# I'll also look at how the percent confidence cutoff affects accuracy. 

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)


# Importing the data ------------------------------------------------------
ground_truth <- read.table(file = file.path(getwd(), "data", "ground_truth_2022-12-12.tsv"),
                           sep = '\t',
                           header = TRUE)

inference <- read.table(file = file.path(getwd(), "data", "2022-12-12_validation_image_predictions.tsv"),
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
  
  return(output_df)
}

df <- get_r_squared(ground_truth, inference)


# Plotting ----------------------------------------------------------------

# Test plot
ggplot(df[df$class == "burst", ], aes(x = hand_count, y = model_count, color = class)) +
  # geom_line(size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 2) +
  geom_point(size = 2) +
  # scale_color_manual(values = color_vec) +
  # scale_y_continuous(breaks = c(0, 0.25, .5, .75, 1), 
                     # labels = c("0%", "25%", "50%", "75%", "100%"),
                     # limits = c(0, 1)) +
  # scale_y_continuous(limits = c(0, 50)) +
  # scale_x_continuous(limits = c(0, 50)) +
  # coord_fixed(ratio = 1, xlim = c(0, 50), ylim = c(0, 50)) +
  coord_fixed(ratio = 1) +
  # labs(title = image_name,
       # y = "Percentage") +
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
        # legend.position = 'none',
        strip.background = element_blank(),
        strip.placement = "outside")

# Stats -------------------------------------------------------------------

summary(lm(hand_count ~ model_count, data = df[df$class == "germinated", ]))$adj.r.squared
summary(lm(hand_count ~ model_count, data = df[df$class == "ungerminated", ]))$adj.r.squared
summary(lm(hand_count ~ model_count, data = df[df$class == "burst", ]))$adj.r.squared
summary(lm(hand_count ~ model_count, data = df[df$class == "aborted", ]))$adj.r.squared


# > summary(lm(hand_count ~ model_count, data = df[df$class == "germinated", ]))$adj.r.squared
# [1] 0.8114968
# > summary(lm(hand_count ~ model_count, data = df[df$class == "ungerminated", ]))$adj.r.squared
# [1] 0.9593114
# > summary(lm(hand_count ~ model_count, data = df[df$class == "burst", ]))$adj.r.squared
# [1] 0.5145596
# > summary(lm(hand_count ~ model_count, data = df[df$class == "aborted", ]))$adj.r.squared
# [1] 0.9237713
