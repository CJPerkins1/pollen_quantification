# Introduction ------------------------------------------------------------
# Playing around with a function that takes bounding box predictions from a 
# neural net (Faster-RCNN ResNet at the moment) and outputs a plot and data 
# frame that show how the percentage of pollen grain categories changes over
# time.

library(dplyr)
library(ggplot2)

df <- read.table(file = file.path(getwd(), "data", "2022-01-05_run1_26C_D2_t082_stab_predictions.tsv"),
                 sep = '\t',
                 header = TRUE)

# filter for just the pollen categories
process_predictions = function(input_df, confidence_threshold) {
  input_df <- input_df %>%
  filter(score >= confidence_threshold) %>%
  group_by(timepoint, class) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count))
}

output_df <- process_predictions(df, 0.5)
