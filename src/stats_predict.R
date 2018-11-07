#This script tries forecast by basic statistics

# load help functions
source("src/help_func.R")

# load functions to predict
source("src/functions.R")

# library import
check.packages(c('data.table', # read data with fread
                 'tibble', # form data like data frame
                 'dplyr', # manipulate data frame
                 'tidyr', # manipulate data frame with dplyr
                 'purrr', # manipulate data frame with dplyr
                 'ggplot2', # make charts
                 'grid', # make charts with ggplot2
                 'lubridate' # deal with date
                 ))

# load data
{rpath <- "data/"
 air_visits <- as.tibble(fread(paste(rpath,'air_visit_data.csv',sep='')))
# air_reserve <- as.tibble(fread(paste(rpath,'air_reserve.csv',sep='')))
# hpg_reserve <- as.tibble(fread(paste(rpath,'hpg_reserve.csv',sep='')))
# air_store <- as.tibble(fread(paste(rpath,'air_store_info.csv',sep='')))
# hpg_store <- as.tibble(fread(paste(rpath,'hpg_store_info.csv',sep='')))
 holidays <- as.tibble(fread(paste(rpath,'date_info.csv',sep='')))
# store_ids <- as.tibble(fread(paste(rpath,'store_id_relation.csv',sep='')))
 test <- as.tibble(fread(paste(rpath,'sample_submission.csv',sep='')))
}

# hava a look at data
head(air_visits, 6)
#head(air_reserve)
#head(hpg_reserve)
#head(air_store)
#head(hpg_store)
head(holidays, 6)
#head(store_ids)
head(test, 6)

# Reformating features
air_visits <- air_visits %>%
    mutate(track_date = ymd(visit_date))

#air_reserve <- air_reserve %>%
#    mutate(visit_datetime = ymd_hms(visit_datetime),
#           reserve_datetime = ymd_hms(reserve_datetime))

#hpg_reserve <- hpg_reserve %>%
#    mutate(visit_datetime = ymd_hms(visit_datetime),
#           reserve_datetime = ymd_hms(reserve_datetime))

#air_store <- air_store %>%
#    mutate(air_genre_name = as.factor(air_genre_name),
#           air_area_name = as.factor(air_area_name))

#hpg_store <- hpg_store %>%
#    mutate(hpg_genre_name = as.factor(hpg_genre_name),
#           hpg_area_name = as.factor(hpg_area_name))

holidays <- holidays %>%
    mutate(holiday_flg = as.logical(holiday_flg),
           track_date = ymd(calendar_date))

#save.image("RData/typed_data.RData")
#load("RData/typed_data.RData")

# average visitors by day of week
visit_dow <- air_visits %>%
    left_join(holidays, by="track_date") %>%
    group_by(day_of_week) %>%
    summarise(avg_visit=mean(visitors))
visit_dow

# median by stores
median_by_stores <- air_visits %>%
    group_by(air_store_id) %>%
    summarise(median=median(visitors))
median_by_stores

# median by stores and day of week
median_by_store_dow <- air_visits %>%
    left_join(holidays, by=c("track_date"="track_date")) %>%
    group_by(air_store_id, day_of_week) %>%
    summarise(median=median(visitors))
median_by_store_dow

holidays %>% group_by(day_of_week) %>% summarise(first)

res <- holidays %>%
    group_by(day_of_week) %>%
    left_join(median_by_store_dow, by="day_of_week") %>%
    select(air_store_id, day_of_week, median) %>%
    arrange(air_store_id)
res

air_visits %>% left_join(holidays, by=c("track_date"="track_date")) %>%
    group_by(air_store_id) %>%
    summarise(cnts=n_distinct(day_of_week)) %>%
    filter(cnts<7)

# prepare submission
head(test)

submit_ids <- parse_test_id(test$id)
dow_submision <- make_submission(submit_ids)
make_submission(submit_ids, model="median_store")

fwrite(make_submission(submit_ids, model="median_store"),
       file="data/median_store_submission.csv",
       quote=FALSE)

# end of file
