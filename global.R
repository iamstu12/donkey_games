
# LIBRARIES --------------------------------------------------------------------

library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinythemes)

# DATA -------------------------------------------------------------------------

# All data ---------------------------------------------------------------------

games <- read_csv("clean_data/clean_sales_data.csv")

# Decade data ------------------------------------------------------------------

decade <- games %>% 
  mutate(year_of_release = as.numeric(year_of_release)) %>% 
  filter(year_of_release != "Unknown") %>% 
  mutate(decade = floor(year_of_release / 10) * 10) %>% 
  filter(decade != 2020) %>% 
  group_by(decade, genre) %>% 
  summarise(global_sales_millions = sum(global_sales)) %>% 
  arrange(decade)


# Genre data -------------------------------------------------------------------

genre <- games %>% 
  mutate(year_of_release = as.numeric(year_of_release)) %>% 
  filter(year_of_release != "Unknown") %>% 
  filter(genre != "Music") %>% 
  mutate(decade = floor(year_of_release / 10) * 10) %>% 
  filter(decade != 2020) %>% 
  group_by(decade, genre) %>% 
  summarise(decade_sales = sum(global_sales)) %>% 
  arrange(genre)

# Regional data ----------------------------------------------------------------

  
regional <- games %>%
  pivot_longer(
    cols = "na_sales" : "other_sales",
    names_to = "region",
    values_to = "sales"
  ) %>% 
  mutate(regional, region = recode(region, na_sales = "North America",
                                   eu_sales = "Europe",
                                   jp_sales = "Japan",
                                   other_sales = "Other")) %>% 
  mutate(year_of_release = as.numeric(year_of_release)) %>% 
  filter(year_of_release != "Unknown") %>% 
  group_by(region, genre) %>% 
  summarise(sales_millions = sum(sales)) %>% 
  arrange(region)

regional %>% 
  ggplot(aes(x = reorder(genre, sales_millions), 
             y = sales_millions)) +
  geom_col(alpha = 0.8, colour = "white", fill = "#cc9900") +
  coord_flip() +
  theme_light() +
  labs(title = "", 
       subtitle = "from 1970 to 2020",
       x = "Genre", 
       y = "Sales (millions)") +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250),
                     expand = c(0.025, 0.025))

regional_2 <- games %>% 
  pivot_longer(
    cols = "na_sales" : "other_sales",
    names_to = "region",
    values_to = "sales"
  ) %>% 
  mutate(regional_2, region = recode(region, na_sales = "North America",
                                eu_sales = "Europe",
                                jp_sales = "Japan",
                                other_sales = "Other"))







