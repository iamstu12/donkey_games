
# Create an object for decade sales -----------------------------------------------

games_decade_data <- games %>% 
  mutate(year_of_release = as.numeric(year_of_release)) %>% 
  filter(year_of_release != "Unknown") %>% 
  mutate(decade = floor(year_of_release / 10) * 10) %>% 
  filter(decade != 2020) %>% 
  group_by(decade, genre) %>% 
  summarise(global_sales_millions = sum(global_sales)) %>% 
  arrange(decade)


# Create an object for top decade games ----------------------------------------

games_top_decade <- games %>% 
  mutate(year_of_release = as.numeric(year_of_release)) %>% 
  filter(year_of_release != "Unknown") %>% 
  mutate(decade = floor(year_of_release / 10) * 10) %>% 
  filter(decade != 2020) %>% 
  group_by(decade, genre, name) %>% 
  summarise(global_sales_millions = sum(global_sales)) %>% 
  arrange(desc(global_sales_millions))




