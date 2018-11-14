# an EDA script by ieiriyuki

pkg <- c('ggplot2', 'dplyr', 'purrr', 'readr', 'stringr')
sapply(pkg, library, character.only=T)

{
    rpath <- "data/"
    air_visits <- as.tibble(fread(str_c(rpath, 'air_visit_data.csv')))
    air_reserve <- as.tibble(fread(str_c(rpath, 'air_reserve.csv')))
    hpg_reserve <- as.tibble(fread(str_c(rpath, 'hpg_reserve.csv')))
    air_store <- as.tibble(fread(str_c(rpath, 'air_store_info.csv')))
    hpg_store <- as.tibble(fread(str_c(rpath, 'hpg_store_info.csv')))
    holidays <- as.tibble(fread(str_c(rpath, 'date_info.csv')))
    store_ids <- as.tibble(fread(str_c(rpath, 'store_id_relation.csv')))
    test <- as.tibble(fread(str_c(rpath, 'sample_submission.csv')))
}

# data type is modified
air_visits <- air_visits %>%
    mutate(visit_date = ymd(visit_date))
air_visits

holidays <- holidays %>%
    mutate(holiday_flg = as.logical(holiday_flg),
           calendar_date = ymd(calendar_date))
holidays

# join by date
air_visits_calendar <- air_visits %>%
    left_join(holidays, by=c("visit_date"="calendar_date"))
air_visits_calendar

# calculate median of each store and day of week
median_by_store_dow <- air_visits_calendar %>%
    group_by(air_store_id, day_of_week) %>%
    summarise(median=median(visitors))
median_by_store_dow

