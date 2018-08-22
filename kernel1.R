# load library

# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# specific visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('ggExtra') # visualisation
library('ggforce') # visualisation
library('viridis') # visualisation

# specific data manipulation
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # string manipulation

# Date plus forecast
library('lubridate') # date and time
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('prophet') # time series analysis
library('timetk') # time series analysis

# Maps / geospatial
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps

# load data
air_visits <- as.tibble(fread(str_c(rpath,'air_visit_data.csv')))
air_reserve <- as.tibble(fread(str_c(rpath,'air_reserve.csv')))
hpg_reserve <- as.tibble(fread(str_c(rpath,'hpg_reserve.csv')))
air_store <- as.tibble(fread(str_c(rpath,'air_store_info.csv')))
hpg_store <- as.tibble(fread(str_c(rpath,'hpg_store_info.csv')))
holidays <- as.tibble(fread(str_c(rpath,'date_info.csv')))
store_ids <- as.tibble(fread(str_c(rpath,'store_id_relation.csv')))
test <- as.tibble(fread(str_c(rpath,'sample_submission.csv')))

