# load required packages
if(!require(shiny)) install.packages('shiny')
if(!require(shinythemes)) install.packages('shinythemes')
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(datasets)) install.packages('datasets')
if(!require(DT)) install.packages('DT')
if(!require(sp)) install.packages('sp')
if(!require(leaflet)) install.packages('leaflet')
if(!require(dplyr)) install.packages('dplyr') 
if(!require(tidyr)) install.packages('tidyr') 
if(!require(lubridate)) install.packages('lubridate') 
if(!require(tidyverse)) install.packages('tidyverse') 
if(!require(shinyWidgets)) install.packages('shinyWidgets')
if(!require(shinyjs)) install.packages('')
if(!require(incidence)) install.packages('')
if(!require(plotly)) install.packages('')
if(!require(googleVis)) install.packages('')
suppressPackageStartupMessages(library(googleVis))
if(!require(htmltools)) install.packages('')
if(!require(geojsonR)) install.packages('')
if(!require(geojsonio)) install.packages('')
if(!require(rgdal)) install.packages('')
if(!require(jsonlite)) install.packages('')


# Prepare data
# 
# load data:
confirmed <- read.table('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv', header = T, sep = ',', quote = '"')
deaths <- read.table('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv', header = T, sep = ',', quote = '"')
recovered <- read.table('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv', header=T, sep=',', quote = '"')

# prepare data:
confirmed <- confirmed %>%
  filter(iso3=='USA') %>%
  gather('date', 'confirmed', -c(1:11)) %>%
  mutate(date = as.Date(date, format='X%m.%d.%y')) %>%
  group_by_at(names(confirmed)[1:11]) %>%
  mutate(new_cases=replace_na(confirmed-lag(confirmed, n=1, order_by=date), 0)) %>%
  ungroup() %>%
  rename(Long=Long_) %>%
  filter(confirmed > 0)

deaths <- deaths %>%
  filter(iso3=='USA') %>%
  gather('date', 'deaths', -c(1:12)) %>%
  mutate(date = as.Date(date, format='X%m.%d.%y')) %>%
  group_by_at(names(deaths)[1:12]) %>%
  mutate(new_deaths=replace_na(deaths-lag(deaths, n=1, order_by=date), 0)) %>%
  ungroup() %>%
  rename(Long=Long_) %>%
  filter(deaths > 0)

recovered <- recovered %>%
  filter(Country.Region == 'US') %>%
  gather('date', 'recovered', -c(1:4)) %>%
  mutate(date = as.Date(date, format='X%m.%d.%y')) %>%
  group_by(.dots=names(recovered)[1:4]) %>%
  mutate(new_recovered=recovered-lag(recovered, n=1, order_by=date)) %>%
  ungroup() 

# aggregate data by state:
confirmed_by_state <- confirmed %>%
  group_by(.dots = c('Province_State', 'Country_Region', 'date')) %>%
  summarise(confirmed=sum(confirmed), new_cases=sum(new_cases)) %>%
  ungroup() %>%
  filter(confirmed > 0) 

confirmed_all_states <- confirmed_by_state %>%
  group_by(.dots=c('Country_Region', 'date')) %>%
  summarise(confirmed=sum(confirmed), new_cases=sum(new_cases)) %>%
  ungroup() %>%
  mutate(Province_State='All States')

n <- names(confirmed_by_state)[order(names(confirmed_by_state))]
confirmed_by_geo <- rbind(confirmed_all_states[,n], confirmed_by_state[,n])

deaths_by_state <- deaths %>%
  group_by(.dots=c('Province_State', 'Country_Region', 'date')) %>%
  summarise(
    deaths=sum(deaths),
    new_deaths=sum(new_deaths),
    population=sum(Population)
  ) %>%
  filter(deaths > 0)

data_by_state <- confirmed_by_state %>%
  left_join(deaths_by_state %>%
              select(-c('population'))) %>%
  mutate(death_rate=ifelse(deaths==0, NA, deaths/confirmed))

# min\max dates:
min_date <- min(confirmed$date)
max_date <- min(c(max(confirmed$date),
                  max(deaths$date),
                  max(recovered$date)
                  )
                )


# load additional states data:
data(state)
states_df <- data.frame(state.x77)
states_df$name <- rownames(states_df)
states_df$Lat <- state.center$y
states_df$Long <- state.center$x

# Load geo-spatial data for interactive maps
url <- 'https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json'
# file_js <- fromJSON('https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json')
states <- geojson_read(url, what = 'sp')
states <- states %>% sp::merge(states_df, by.x="NAME", by.y='name')

# add location data to the merged data:
data_by_state <- data_by_state %>% 
  left_join(states_df %>%
              select(c('name', 'Lat', 'Long')),
            by=c('Province_State'='name')) %>%
  mutate(labs=paste(Province_State, ": ",
                    formatC(confirmed, big.mark = ","), ' cases<br/>death rate: ',
                    ifelse(confirmed==0,NA,round(death_rate*100,2)), '%'))

# create base leaflet map:
map_base <- leaflet() %>%
  setView(-98.483330, 38.712046, zoom = 3) %>% 
  addTiles() 

# gvis data preparation:
population <- read.csv('SCPRC-EST2019-18+POP-RES.csv')
df_gvis <- confirmed_by_state %>%
  left_join(deaths_by_state %>%
              select(-c(population))) %>%
  left_join(population %>%
              select('NAME', 'POPESTIMATE2019') %>%
              rename(population=POPESTIMATE2019), 
            by=c('Province_State'='NAME')) %>% 
  replace_na(list('deaths'=0, 'new_deaths'=0)) %>%
  mutate('death_rate_%'=round(deaths/ifelse(confirmed==0, NA, confirmed)*100, 2),
         'new_cases_%'=round(new_cases/ifelse((confirmed-new_cases)==0, NA, confirmed-new_cases)*100, 2)) %>%
  replace_na(list('death_rate_%'=0, 'new_cases_%'=0))

