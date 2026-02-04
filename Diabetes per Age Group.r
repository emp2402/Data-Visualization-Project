library(dplyr)
library(ggplot2)

# Step 1: load the data
df = read.csv("diabetes.csv")

# Step 2 & 3: Break Age into 5-year subgroups and compute counts
results <- df %>%
  mutate(AgeGroup = floor(Age / 5) * 5) %>% # Creates bins: 20, 25, 30, etc.
  group_by(AgeGroup) %>%
  summarize(
    total_diabetic = sum(Diabetes == 1),
    group_size = n()
  ) %>%
  mutate(fraction_diabetic = total_diabetic / group_size)

# Step 4: Create the labels manually
ticks <- seq(0, 1, by = 0.2)             # Creates 0.0, 0.2, 0.4, 0.6, 0.8, 1.0
percent_labels <- paste0(ticks * 100, "%") # Converts to "0%", "20%", etc.

# Step 5: Start the plot but suppress the default y-axis label (ylab = "")
plot(results$AgeGroup, results$fraction_diabetic,
     type = "p", 
     pch = 19, 
     col = "darkblue",
     xlab = "Age (5-year Groups)", 
     ylab = "",
     ylim = c(0, 1),
     main = "Probability of Diabetes by Age Group",          
     cex.axis = 0.7,
     frame.plot = FALSE,
     yaxt = "n")

# Step 6: Draw a custom Y-axis
axis(side = 2, 
     at = ticks, 
     labels = percent_labels, 
     cex.axis = 0.7, 
     las = 1)   # las = 1 makes the percentages horizontal (easier to read)

# Step 7: Use mtext to manually place the label
# side = 2 (left axis)
# line = 2 (lower numbers move the label closer to the axis/right)
# cex = 1 (controls the font size of the label itself)
mtext("Percentage of Diabetics", side = 2, line = 2.5, cex = 1)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")