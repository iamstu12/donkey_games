
# Load libraries and data ------------------------------------------------------

library(tidyverse)
library(janitor)

sales_2016_raw <- read_csv("raw_data/sales-2016-with-ratings.csv") %>% 
  clean_names()

sales_2019_raw <- read_csv("raw_data/sales-2019.csv") %>% 
  clean_names()

sales_ps4_raw <- read_csv("raw_data/ps4-game-sales.csv") %>% 
  clean_names()

sales_xbox_raw <- read_csv("raw_data/xbox-one-game-sales.csv") %>% 
  clean_names()

# Examine 'sales_2016_raw' data ------------------------------------------------

glimpse(sales_2016_raw)
head(sales_2016_raw)

# Count missing values in each column:

colSums(is.na(sales_2016_raw))

# Name: missing 2, coalesce to 'Unknown'
# Genre: missing 2, coalesce to 'other'
# Critic_Score, Critic_Count, User_Score, User_Count, : all missing a 
  # large number, drop columns
# Developer, Rating: missing a lot, coalesce to 'unknown'

sales_2016_clean <- sales_2016_raw %>% 
  replace_na(list(name = "Unknown",
                  developer = "Unknown",
                  rating = "Unknown",
                  genre = "Other",
                  year_of_release = "Unknown"))

# Drop unwanted columns:

sales_2016_clean <- sales_2016_clean %>% 
  select(-c(critic_score, critic_count, user_score, user_count, rating))

# I dropped the 'score' and 'count' columns as I am not that interested in 
  # them, I am more concerned with the number of sales.

colSums(is.na(sales_2016_clean))

# There are now no missing values

# Rename columns

sales_2016_clean <- sales_2016_clean %>% 
  rename(console = platform)

names(sales_2016_clean)

# Check out the genre column

unique(sales_2016_clean$genre)

# change 'misc' to 'other'

sales_2016_clean <- sales_2016_clean %>% 
  mutate(genre = recode(genre, "Misc" = "Other"))

unique(sales_2016_clean$genre)

# Add column for 'year_of_sales':

sales_2016_clean <- sales_2016_clean %>% 
  mutate(sales_year = 2016)


# Create a data set that only contains 'shipped' details -----------------------

shipped_2019_clean <- sales_2019_raw %>% 
  filter(!is.na(total_shipped))

# Select relevant columns:

shipped_2019_clean <- shipped_2019_clean %>% 
  select(c(name, genre, platform, publisher, developer, total_shipped, year))

names(shipped_2019_clean)

# Change name of platform to 'console':

shipped_2019_clean <- shipped_2019_clean %>% 
  rename(console = platform)

# Check for missing values:

colSums(is.na(shipped_2019_clean))

# There are no missing values


# Examine 'sales_2019_raw' data ------------------------------------------------

# Remove the shipped details:

sales_2019_clean <- sales_2019_raw %>% 
  filter(!is.na(global_sales))

# Select relevant columns:

names(sales_2019_clean)

sales_2019_clean <- sales_2019_clean %>% 
  select(c(name, genre, platform, publisher, developer, global_sales, 
           na_sales, pal_sales, jp_sales, other_sales, year))

# Check for missing values:

colSums(is.na(sales_2019_clean))

# Check out the genre column

unique(sales_2019_clean$genre)

# Change 'misc' to 'other':

sales_2019_clean <- sales_2019_clean %>% 
  mutate(genre = recode(genre, "Misc" = "Other",
                        "Action-Adventure" = "Action",
                        "Music" = "Other",
                        "MMO" = "Other",
                        "Sandbox" = "Other",
                        "Party" = "Other",
                        "Board Game" = "Other",
                        "Visual Novel" = "Other",
                        "Education" = "Other",
                        ))

# Rename columns:

sales_2019_clean <- sales_2019_clean %>% 
  rename(eu_sales = pal_sales) %>% 
  rename(console = platform) %>% 
  rename(year_of_release = year)

# Add column for 'year_of_sales':

sales_2019_clean <- sales_2019_clean %>% 
  mutate(sales_year = 2019)

colSums(is.na(sales_2019_clean))

# Replace missing values:

sales_2019_clean <- sales_2019_clean %>% 
  replace_na(list(developer = "Unknown",
                  na_sales = 0,
                  eu_sales = 0,
                  jp_sales = 0,
                  other_sales = 0,
                  year_of_release = "Unknown"))

colSums(is.na(sales_2019_clean))  

# Check column names match 2016 sales:

names(sales_2016_clean)
names(sales_2019_clean)

# Change year column to factor class:

sales_2016_clean <- sales_2016_clean %>% 
  mutate(year_of_release = as.factor(year_of_release))

sales_2019_clean <- sales_2019_clean %>% 
  mutate(year_of_release = as.factor(year_of_release))


sales_all_clean <- bind_rows(sales_2016_clean, sales_2019_clean)

colSums(is.na(sales_all_clean))

# X Box and PS4 data -----------------------------------------------------------

names(sales_ps4_raw)
names(sales_xbox_raw)

# Add new column to each data set

sales_ps4_clean <- sales_ps4_raw %>% 
  mutate(console = "PS4")

sales_xbox_clean <- sales_xbox_raw %>% 
  mutate(console = "XOne")

# Drop 'pos' column from xbox_clean:

sales_xbox_clean <- sales_xbox_clean %>% 
  select(-pos)
  
names(sales_ps4_clean)
names(sales_xbox_clean)

# Join data:

sales_xbox_ps4_clean <- bind_rows(sales_ps4_clean, sales_xbox_clean)

# Check for missing values:

colSums(is.na(sales_xbox_ps4_clean))

# Replace missing values:

sales_xbox_ps4_clean <- sales_xbox_ps4_clean %>% 
  replace_na(list(publisher = "Unknown"))

# Rename columns to match other data sets:

names(sales_all_clean)

sales_xbox_ps4_clean <- sales_xbox_ps4_clean %>% 
  rename(name = game) %>% 
  rename(year_of_release = year) %>% 
  rename(na_sales = north_america) %>% 
  rename(eu_sales = europe) %>% 
  rename(jp_sales = japan) %>% 
  rename(other_sales = rest_of_world) %>% 
  rename(global_sales = global)

# Recap:

#1. sales_all_clean - this is a combination of both 2016 and 2019 sales
#2. shipped_2019_clean - this only contains shipped values for 2019
#3. sales_xbox_ps4_clean - this contain sales for unknown year

# note - shipped is the amount of units sent by the publisher to retail

# Write clean data to folder:

write_csv(sales_all_clean, "clean_data/sales_all_clean")
write_csv(shipped_2019_clean, "clean_data/shipped_2019_clean")
write_csv(sales_xbox_ps4_clean, "clean_data/sales_xbox_ps4_clean")





  


