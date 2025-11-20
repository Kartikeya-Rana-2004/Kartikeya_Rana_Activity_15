# Activity 8 Question 5
library(tidyverse)

armed_forces <- read_csv("US.csv")
glimpse(armed_forces)
View(armed_forces)
names(armed_forces)
groups_df <- armed_forces %>%
  pivot_longer(
    cols = -`Pay Grade`,  
    names_to = "Category",
    values_to = "Count"
  ) %>%
  
  separate(Category, into = c("Branch", "Sex"), sep = " ", extra = "merge") %>%
  filter(!is.na(Count), Count > 0) %>%
  mutate(
    Branch = str_trim(Branch),
    Sex = str_trim(Sex),
    Pay_Grade = `Pay Grade`
  ) %>%
  select(Pay_Grade, Branch, Sex, Count)
individuals_df <- groups_df %>%
  uncount(weights = Count)

original_total <- armed_forces %>%
  select(-`Pay Grade`) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  rowSums()

groups_total <- sum(groups_df$Count, na.rm = TRUE)
individuals_total <- nrow(individuals_df)







