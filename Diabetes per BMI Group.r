library(tidyverse)
library(scales)

#Step 1: load the data
df = read.csv("diabetes.csv")

breaks <- seq(15, 35, by = 2)

# Step 2: create the BMI subgroups and remove the outliers (BMI > 35)
df <- df %>% filter(BMI <= 35) %>%
  mutate(BMI_group = cut(BMI, 
                         breaks = breaks, 
                         right = FALSE, 
			 include.lowest = TRUE,
                         labels = c("15-17", "17-19", "19-21", "21-23", "23-25", 
                                    "25-27", "27-29", "29-31", "31-33", "33-35")))

# Step 2 & 3: Compute averages and the final fraction
plot_data <- df %>% 
  group_by(BMI_group) %>%
  summarise(
    total_diabetic = sum(Diabetes == 1),
    group_size = n(),
    # Point 3: Calculate the fraction (this will equal avg_diabetic)
    fraction_diabetic = total_diabetic / group_size 
  )

# Step 4: Plot the results
ggplot(plot_data, aes(x = BMI_group, y = fraction_diabetic)) +
  geom_col(fill = "steelblue", color = "white") +
  geom_text(aes(label = round(fraction_diabetic, 2)), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Fraction of Diabetics by BMI Subgroup",
    x = "BMI Range",
    y = "Percentage of Diabetics"
  ) +
  theme_minimal() +
  theme(
	axis.text.x = element_text(angle = 45, vjust = 1.5, hjust = 1),
	axis.title.y = element_text(margin = margin(r = 15))
)