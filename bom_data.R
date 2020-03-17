# load tidyverse library

library(tidyverse)

# read data

bom_data <- read.csv('data/BOM_data.csv')
bom_stations <- read.csv('data/BOM_stations.csv')

# Question 1 - For each station, how many days have a minimum temperature,
# a maximum temperature and a rainfall measurement recorded?

temp_min_max_sep <- separate(bom_data, Temp_min_max, into = c('temp_min', 'temp_max'),sep= '/') %>% 
filter(temp_min != '-', temp_max != '-', Rainfall != '-') 
