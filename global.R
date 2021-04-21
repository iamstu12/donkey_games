
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
  )

regional <- regional %>%
  mutate(regional, region = recode(region, na_sales = "North America",
                                   eu_sales = "Europe",
                                   jp_sales = "Japan",
                                   other_sales = "Other")) %>% 
  mutate(year_of_release = as.numeric(year_of_release)) %>% 
  filter(year_of_release != "Unknown") %>% 
  group_by(region, genre) %>% 
  summarise(sales_millions = sum(sales)) %>% 
  arrange(region)

regional_2 <- games %>% 
  pivot_longer(
    cols = "na_sales" : "other_sales",
    names_to = "region",
    values_to = "sales"
  )

regional_2 <- regional_2 %>%
  mutate(regional_2, region = recode(region, na_sales = "North America",
                                eu_sales = "Europe",
                                jp_sales = "Japan",
                                other_sales = "Other"))

# Console data ---------------------------------------------------------------

console <- games %>% 
  mutate(year_of_release = as.numeric(year_of_release)) %>% 
  filter(year_of_release != "Unknown") %>% 
  filter(genre != "Music") %>% 
  mutate(decade = floor(year_of_release / 10) * 10) %>% 
  filter(decade != 2020) %>% 
  pivot_longer(
    cols = "na_sales" : "other_sales",
    names_to = "region",
    values_to = "sales"
  )

console <- console %>% 
  mutate(top_games, region = recode(region, na_sales = "North America",
                                   eu_sales = "Europe",
                                   jp_sales = "Japan",
                                   other_sales = "Other"))

console <- console %>% 
  mutate(console, console = recode(console, Wii = "Nintendo Wii",
                                     NES = "Nintendo NES",
                                     GB = "Nintendo GameBoy",
                                     DS = "Nintendo DS",
                                     X360 = "XBox 360",
                                     PS2 = "PlayStation 2",
                                     SNES = "Nintendo SNES",
                                     PS3 = "PlayStation 3",
                                     PS4 = "PlayStation 4",
                                     GBA = "Nintendo GameBoy Advance",
                                     "3DS" = "Nintendo 3DS",
                                     N64 = "Nintendo 64",
                                     PS = "PlayStation 1",
                                     XOne = "XBox One",
                                     XB = "XBox",
                                     "2600" = "Atari 2600",
                                     PSP = "PlayStation Portable",
                                     WiiU = "Nintendo WiiU",
                                     GC = "Nintendo GameCube",
                                     GEN = "Sega Genesis",
                                     NS = "Nintendo Switch",
                                     PSN = "PlayStation Network",
                                     PSV = "PlayStation Vita",
                                     DC = "Sega Dreamcast",
                                     SAT = "Sega Saturn",
                                     GBC = "Nintendo GameBoy Color",
                                     SCD = "Sega CD",
                                     WS = "WonderSwan",
                                     VC = "Nintendo Virtual Console",
                                     NG = "Neo Geo",
                                     WW = "Nintendo Wii",
                                     PCE = "TurboGrafx-16",
                                     TG16 = "TurboGrafx-16",
                                     XBL = "XBox Live",
                                     "3DO" = "Panasonic FZ-1",
                                     GG = "Sega Game Gear",
                                     OSX = "PC",
                                     PCFX = "PC-FX",
                                     Mob = "PC"))


console2 <- console %>% 
  group_by(console, genre) %>% 
  summarise(sales_millions = sum(sales))


  
  



  






