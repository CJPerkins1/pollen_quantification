# Introduction ------------------------------------------------------------
# Playing around with a function that takes bounding box predictions from a 
# neural net (Faster-RCNN ResNet at the moment) and outputs a plot and data 
# frame that show how the percentage of pollen grain categories changes over
# time.

library(dplyr)
library(ggplot2)

process_data_make_plot = function(name){
  df <- read.table(file = file.path(getwd(), "data", paste0(name, "_stab_predictions.tsv")),
                   sep = '\t',
                   header = TRUE)
  
  # Processing
  process_predictions = function(input_df, confidence_threshold) {
    input_df <- input_df %>%
      filter(score >= confidence_threshold) %>%
      filter(class %in% c("germinated", "burst", "ungerminated", "aborted", "unknown_germinated")) %>%
      group_by(timepoint, class) %>%
      summarize(count = n()) %>%
      mutate(percentage = count / sum(count))
  }
  
  output_df <- process_predictions(df, 0.5)
  
  # Plotting
  make_germination_plot <- function(input_df, image_name) {
    #color_vec <- c("#2F69FF", "#DC267F", "#FFB000", "#5fc77b") # Germinated, burst, 
    # This will only work if there's no aborted but there are all the other ones, 
    # make it do the classes right if there's not always all of them

    # FIX COLORS
    color_vec <- c("#DC267F", "#5fc77b", "#2F69FF", "#FFB000") # Burst, germinated, ungerminated, unknown_germinated
    
    
    ggplot(input_df, aes(x = timepoint, y = percentage, color = class)) +
      geom_line(size = 2) +
      geom_point(size = 2) +
      scale_color_manual(values = color_vec) +
      #scale_y_continuous(expand = expansion(mult = c(0, .05))) +
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
  
  make_germination_plot(output_df, name)
}

process_data_make_plot("2022-01-05_run1_26C_D2_t082")
process_data_make_plot("2022-01-06_run1_34C_A4_t082")
process_data_make_plot("2022-01-06_run1_34C_A5_t082")
process_data_make_plot("2022-02-18_run1_34C_C5_t082")
process_data_make_plot("2022-02-21_run1_26C_C5_t082")
process_data_make_plot("2022-02-21_run1_26C_D3_t082")
