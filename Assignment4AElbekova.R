#Aidai Elbekova
# Assignment 4

# Task 1 
# Download the dataset demographics.csv from moodle to your working directory, 
# and import the csv file to your R-studio (assign a name to your dataframe).
demographics_df <- read.csv("../desktop/text_mining/datasets/demographics.csv", sep = ";")

# Task 2
# Is the dataframe in tidy format? If it is, explain your reasoning. 
# If not, transform the dataframe in tidy format.
library("tidyverse")
demographics_tidy <- spread(demographics_df, Age, Population) %>% 
  rename_with( ~ paste0("Age ", .x), .cols = 4:21)

# Task 3
# Return the number of men living in Germany in 2020, whose age are between 50 and 54.
library(dplyr)
demographics_tidy %>% 
  filter (Country=="Germany", Sex=="M", Year=="2020") %>% 
  select("Age 50 to 54")

# Task 4
# Return the number of women living in Italy in 2019, 
# whose age are either between 25 and 29 or 35 and 39.
demographics_tidy %>% 
  filter (Country=="Italy", Sex=="W", Year=="2019") %>% 
  select("Age 25 to 29"| "Age 35 to 39") %>%
  sum()

# Task 5
# Create a dataframe which shows the total population for each year and each country. 
# (Hint: The dataframe should have two columns: Country and Total_population.)
demographics_tidy_2 <- demographics_tidy %>%
  mutate(Population_age_aggregate= select(., `Age 0 to 4`:`Age 85 and over`) %>% rowSums()) %>%
  select(Country, Year, Population_age_aggregate) %>%
  group_by(Country, Year) %>%
  summarise(Total_population = sum(Population_age_aggregate))


# Task 6
# Create a dataframe which shows the number of people aged 65 or over, for each country, 
# year and sex. (Hint: The dataframe should have four columns: Country, Year, Sex, and Pop_over65)
demographics_tidy_3 <- demographics_tidy %>% 
  mutate(Pop_over65 = select(., `Age 65 to 69`:`Age 85 and over`) %>% rowSums()) %>%
  select(Country, Year, Sex, Pop_over65)

# Task 7
# Create a dataframe which shows old age dependency ratio and total dependency ratio for all 
# countries and years (formulas below) and order the dataframe based on the country names, alphabetically. 
# (Hint: There should be four columns in the dataframe: Country, Year, Old_age_dep, Total_dep.)
demographics_tidy_4 <- demographics_tidy %>% 
  mutate(Pop_14_64 = select(., `Age 15 to 19`:`Age 60 to 64`) %>% rowSums()) %>% 
  mutate(Pop_over65 = select(., `Age 65 to 69`:`Age 85 and over`) %>% rowSums()) %>% 
  mutate(Old_age_dep = Pop_over65/Pop_14_64) %>% 
  mutate(Pop_below_20 = select(., `Age 0 to 4`:`Age 15 to 19`) %>% rowSums()) %>% 
  mutate(Pop_20_64 = select(., `Age 20 to 24`:`Age 60 to 64`) %>% rowSums()) %>% 
  mutate(Total_dep = (Pop_below_20+Pop_over65)/Pop_20_64) %>% 
  select(Country, Year, Old_age_dep, Total_dep)

# Task 8
# Download and read the gdpc_data.csv file. By using one of the join functions, recreate the following table. 
# (Make sure the you end up with the exact same table; values match and variable names are the same.)
gdpc_data <- read.csv("../desktop/text_mining/datasets/gdpc_data.csv", sep = ";")
demographics_tidy_2_a <- demographics_tidy_2 %>%
  filter(Year=="2019"| Year == "2020") %>%
  filter(Country == "Austria"|Country ==  "Belgium"|Country == "France"| Country == "Netherlands")
grpc_data_join <- inner_join(demographics_tidy_2_a, gdpc_data, key="Year")

# Task 9
# Based on the demographics dataset, recreate the following plot.
library(ggplot2)
demographics_tidy_2_b <- demographics_tidy %>%
  mutate(Pop_total= select(., `Age 0 to 4`:`Age 85 and over`) %>% rowSums()) %>%
  select(Country, Sex, Pop_total)
ggplot(demographics_tidy_2_b, aes(x=Country, y=Pop_total, fill=Sex)) + 
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(values = c('springgreen4','chocolate'))

# Task 10
# Based on the demographics dataset, recreate the following plot.
demographics_tidy_2_c <- demographics_tidy %>%
  mutate(Pop_total= select(., `Age 0 to 4`:`Age 85 and over`) %>% rowSums()) %>%
  select(Country, Year, Pop_total)
ggplot(demographics_tidy_2_c, aes(x=Year, y=Pop_total, color=Country)) + 
  geom_line()
