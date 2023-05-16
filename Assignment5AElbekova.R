# Aidai Elbekova
# Assignment 5

# Upload CSV

officesupplies <- read.csv("../desktop/text_mining/datasets/officesupplies.csv", sep = ";")

# Upload libraries
library(tidyverse)
library(ggplot2)
library(stringr)


# Remove "_"/ "-"/",,"/ "*"/ "'" 
officesupplies$Person <- chartr("_", " ", officesupplies$Person)
officesupplies$Person <- chartr("-", " ", officesupplies$Person)
officesupplies$Person <- chartr(",", " ", officesupplies$Person)
officesupplies$Person <- chartr("'", " ", officesupplies$Person)
officesupplies$Person <- chartr("*", " ", officesupplies$Person)

# Trim extra white space in Persons
str_squish(officesupplies$Person)

# Split "Persons" to two columns
officesupplies[c("Name", "Surname")] <- str_split_fixed(officesupplies$Person," ",2)

# Capital Letters in Names & Surnames
officesupplies$Name <- str_to_title(officesupplies$Name)
officesupplies$Surname <- str_to_title(officesupplies$Surname)

# Remove "Company" in column "Company"/ 'company'
officesupplies$Company <-str_replace_all(officesupplies$Company,"[C]ompany$","")
officesupplies$Company <-str_replace_all(officesupplies$Company,"[c]ompany$","")

# Phone number & email
officesupplies$Contact <- gsub(pattern="^[[:alnum:].-_]+@[[:alnum:].-]+$", 
                                   x=officesupplies$Mail_or_Phone, replacement="Mail") 

officesupplies$Contact <-  gsub(pattern="[0-9]{11}", 
                               x=officesupplies$Mail_or_Phone, replacement="Phone")
# Create column "Amount"
officesupplies$Amount <- gsub("[- .)(+]|[a-zA-Z]*:?","", officesupplies$Message)

# Create column "Color"
officesupplies$Message <- str_to_lower(officesupplies$Message)
colors_table <- c("blue", "black", "yellow", "red", "white" , "green")
officesupplies$Color <- str_extract(officesupplies$Message, paste(colors_table, collapse="|" ))
officesupplies$Color <- str_to_title(officesupplies$Color)   

# Create column Product
product_table <- c("gel pens", "staples", "permanent markers", "business envelopes",
                   "calendars", "filler paper", "sticky notes", "file folders")
officesupplies$Product <- str_extract(officesupplies$Message, paste(product_table, collapse="|" ))
officesupplies$Product <- str_to_title(officesupplies$Product)

# Create final data frame
officesupplies_df <- officesupplies %>%
  select(Company, Surname, Name, Contact, Product, Color, Amount)




  
