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

rpath <- 'data/'
air_visits <- read_csv(str_c(rpath,'air_visit_data.csv'))
class
