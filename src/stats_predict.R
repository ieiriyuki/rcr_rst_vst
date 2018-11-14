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

# extract ids for submission
submit_ids <- parse_test_id(test$id)

#save.image("RData/typed_data.RData")
#load("RData/typed_data.RData")

# average visitors by day of week
visit_dow <- air_visits %>%
    left_join(holidays, by="track_date") %>%
    group_by(day_of_week) %>%
    summarise(avg_visit=mean(visitors))
visit_dow

# make submission by mean of visitor for day of week
# no longer available
# dow_submision <- make_submission(submit_ids)

# median by stores
median_by_store <- air_visits %>%
    group_by(air_store_id) %>%
    summarise(median=median(visitors))
median_by_store

# make submission by median of visitors for each store
# this is a benchmark.score
fwrite(make_submission(submit_ids, model="median_store"),
       file="data/median_store_submission.csv",
       quote=FALSE)

# median by stores and day of week
median_by_store_dow <- air_visits %>%
    left_join(holidays, by=c("track_date"="track_date")) %>%
    group_by(air_store_id, day_of_week) %>%
    summarise(median=median(visitors)) %>%
    ungroup()
median_by_store_dow

# check if any restaurant has no visitor
air_visits %>% left_join(holidays, by=c("track_date"="track_date")) %>%
    group_by(air_store_id) %>%
    summarise(cnts=n_distinct(day_of_week)) %>%
    filter(cnts<7)

# fill median missing for air_store_id and day_of_week
ids <- tibble(id=submit_ids) %>%
    mutate(id, num=1)

dow <- holidays %>%
    group_by(day_of_week) %>%
    select(day_of_week) %>%
    distinct() %>%
    ungroup() %>%
    mutate(day_of_week, num=1)

test.tbl <- ids %>%
    left_join(dow, by="num") %>%
    select(c("id", "day_of_week")) %>%
    mutate(air_store_id=id) %>%
    select(c("air_store_id", "day_of_week"))
test.tbl

# this still contains NA
all_median_by_store_dow <- test.tbl %>%
    left_join(median_by_store_dow, by=c("air_store_id"="air_store_id",
                                        "day_of_week"="day_of_week"))
all_median_by_store_dow

# calculate mean to fill
mean_by_store <- all_median_by_store_dow %>%
    group_by(air_store_id) %>%
    summarise(mean=mean(median, na.rm=TRUE))
mean_by_store

# fill NA here
for( i in 1:nrow(all_median_by_store_dow) ){
    if( is.na(all_median_by_store_dow[i,]$median) ){
        all_median_by_store_dow[i,]$median <- 
            mean_by_store[
                mean_by_store$air_store_id==all_median_by_store_dow[i,
                        ]$air_store_id,
                ]$mean
    }
}

# make submission by median for each store and each day of week
temp <- make_submission(submit_ids, model="median_store_dow")
temp

fwrite(temp,
       file="data/median_store_dow_submission.csv",
       quote=FALSE)


# prepare submission
head(test)


# end of file
