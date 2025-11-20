library(tidyverse)
branch_sex <- c(
  "Army_Male", "Army_Female", "Army_Total",
  "Navy_Male", "Navy_Female", "Navy_Total",
  "Marine_Male", "Marine_Female", "Marine_Total",
  "AirForce_Male", "AirForce_Female", "AirForce_Total",
  "SpaceForce_Male", "SpaceForce_Female", "SpaceForce_Total",
  "Total_Male", "Total_Female", "Total_Total"
)

col_names <- c("Pay_Grade", branch_sex)
data <- read_csv("US.csv", skip=2, col_names = col_names)
data <- data %>% filter(!(Pay_Grade %in% c("Total Enlisted","Total Warrant Officers","Total Officers","Total")))
long_data <- data %>%
  pivot_longer(-Pay_Grade, names_to = c("Branch", "Sex"), names_pattern = "(.*)_(Male|Female|Total)", values_to = "Count") %>%
  filter(Sex %in% c("Male", "Female"), !is.na(Count))
long_data$Count <- as.numeric(gsub(",", "", long_data$Count))

freq_table <- function(df, sex_label) {
  sex_data <- df %>% filter(Sex==sex_label)
  table <- sex_data %>%
    group_by(Pay_Grade, Branch) %>%
    summarise(Count = sum(Count, na.rm=TRUE), .groups='drop') %>%
    group_by(Pay_Grade) %>% 
    mutate(TotalRow = sum(Count),
           RelFreq = round(Count/TotalRow*100,2)) %>%
    ungroup()
  return(table)
}

male_freq <- freq_table(long_data, "Male")
female_freq <- freq_table(long_data, "Female")
print(male_freq)
print(female_freq)
