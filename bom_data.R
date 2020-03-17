# load tidyverse library

library(tidyverse)

# read data

bom_data <- read.csv('data/BOM_data.csv')
bom_stations <- read.csv('data/BOM_stations.csv')

# Question 1 - For each station, how many days have a minimum temperature,
# a maximum temperature and a rainfall measurement recorded?

temp_min_max_sep <- separate(bom_data, Temp_min_max, into = c('temp_min', 'temp_max'),sep= '/') %>% 
filter(temp_min != '-', temp_max != '-', Rainfall != '-') -> stations_max_min_rain

view(stations_max_min_rain)

# Question 2 - Which MONTH saw the lowest average daily temperature difference?

average_temp <- mutate(temp_min_max_sep,Temp_diff = as.numeric(temp_max)-as.numeric(temp_min)) %>% 
group_by(Month) %>% 
summarise(temp_mean = mean(Temp_diff, na.rm = TRUE)) %>% 
arrange(temp_mean)

# Question 3 Does the westmost (lowest longitude) or eastmost (highest longitude)
# weather station in our dataset have a higher average solar exposure?


