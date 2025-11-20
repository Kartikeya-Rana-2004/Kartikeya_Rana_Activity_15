#R code for the appropriate data wrangling of the BabyNames data frame to focus on just the names you selected

library(dcData)
library(dplyr)
selected_names <- c("Emma", "Sophia", "Jennifer", "Margaret")
BabyNames_filtered <- BabyNames %>%
  filter(name %in% selected_names) %>%
  group_by(year, name) %>%
  summarise(total_count = sum(count), .groups = 'drop')


# R code for your data visualization showing the popularity of your selected names over time.

library(ggplot2)
ggplot(BabyNames_filtered, aes(x = year, y = total_count, color = name, linetype = name)) +
  geom_line(size = 1) +
  labs(
    title = "Popularity of Selected Female Names Over Time",
    x = "Year",
    y = "Total Number of People with Name",
    color = "Name",
    linetype = "Name"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90")
  )


