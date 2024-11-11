###################################
#### For EDA Section of Report ####
###################################
library(tidyverse)
library(ggplot2)
library(usmap)
#project_directory <- "~/Documents/School/BIOS511/Project"
project_directory <- "~/Documents/2024.03 - Fall/BIOS511/Final Project/Hospital-Privatization" # replace this with your own project directory
PES_path <- "/data/PE Hospital Tracker.csv"
PES_data <- read_csv(paste(project_directory, PES_path, sep = ""))

# This is the number of hospitals currently owned by Private Equity
# and the total number tracked
count(PES_data %>% filter(`Currently PE?`=="Y"))
count(PES_data)

# A table containing the number of hospitals owned by different PE firms
PES_data %>% 
  filter(`Currently PE?`=="Y") %>% 
  count(`PE Firm`, sort = T) 

# Plots the percentage of PE owned hospitals by state
PES_state = PES_data %>%
  group_by(State) %>%
  summarize(percent_PE = mean(`Currently PE?` == "Y") * 100, total = n()) %>%
  rename(state = State)
plot_usmap(regions = "states", data = PES_state, values = "percent_PE") + 
  labs(title = "Private Equity Ownership of Hospitals in US States") + 
  scale_fill_continuous(low = "white", high = "red", name = "Percentage owned by PE") +
  theme(legend.position = "right")

# Plots total hospitals versus percent PE
ggplot(data = PES_state, aes(x=percent_PE, y=total)) + 
  geom_point() + 
  xlab("Percentage of Hospitals Owned by PE") + ylab("Total Number of Hospitals") +
  theme_minimal()
