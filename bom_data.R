# load tidyverse library

library(tidyverse)

# read data

bom_data <- read_csv('data/BOM_data.csv')
bom_stations <- read_csv('data/BOM_stations.csv')

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

# Question 3 Which state saw the lowest average daily temperature difference

# STEP1 gather and spread data

bom_station_data <- bom_stations %>% 
  gather(Station_ID, Misc, -info) %>% 
  spread(info, Misc)
  

# STEP2 convert temp min and max from chr to numbers

temp_min_max_sep <- mutate(temp_min_max_sep, temp_min = as.numeric(temp_min))
temp_min_max_sep <- mutate(temp_min_max_sep, temp_max = as.numeric(temp_max))
bom_station_data <- mutate(bom_station_data, Station_number = as.numeric(Station_ID))

# STEP3 join the two data sets

combined_bom_data <- full_join(temp_min_max_sep, bom_station_data, 'Station_number')

# STEP4 sort by state and arrange

lowest_average_temp <- group_by(combined_bom_data, state) %>%
  summarise(lowest_average_temp = mean(temp_min, na.rm = TRUE)) %>%
  arrange(lowest_average_temp)

View(lowest_average_temp)
  
# Question 4 Does the westmost (lowest longitude) or eastmost (highest longitude)
# weather station in our dataset have a higher average solar exposure?

# STEP WHATEVER! group by station number and longitude and state.
# get the mean of Solar_exposure
# arrange by longitude. Save. Commit. Push. Have a stiff drink and go to bed!
# Next morning - ARRRRGH! redo!

bom_solar_exp <- combined_bom_data
group_by(bom_solar_exp, lon, state, Station_number)

# get mean of solar exposure.

summarise(bom_solar_exp, Solar_exposure = mean(as.numeric(Solar_exposure)), na.rm = TRUE)
bom_solar_exp <- filter(bom_solar_exp, Solar_exposure != '-')

# arrange by lon.

ungroup(bom_solar_exp)
arrange(bom_solar_exp, desc(lon))

# return first and last rows.

Question4_answer <- slice(bom_solar_exp, 1,(n()-1))
view(Question4_answer)
