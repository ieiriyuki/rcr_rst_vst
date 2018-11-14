# functions to make prediction

make_submission <- function(ids,
                            start=as.Date("2017-04-23"),
                            end=as.Date("2017-05-31"),
                            model="median_store"){

    day_range <- seq(start, end, by="days")
    id_reps <- rep(ids, each=length(day_range))
    day_reps <- rep(day_range, times=length(ids))
    
    # here putting visitors for id x date
    visitors <- map2_dbl(id_reps,
                         day_reps,
                         predict_visitors,
                         model=model)

    # concatenate id and date
    id <- map2_chr(id_reps, day_reps, paste, sep="_")

    # create table for submission
    submit_table <- data.table(id=id,
                               visitors=visitors)

    return(submit_table)
}

predict_visitors <- function(id, date_, model="median_store"){
    # null hypothesis
    visitors <- 1

    if(model=="median_store"){
        return(predict_by_median_by_store(id))
    }else if(model=="median_store_dow"){
        return(predict_by_median_by_store_dow(id, date_))
    }

    return(visitors)
}

predict_by_median_by_store <- function(id){
    # refer to global tibble
    v <- select(filter(median_by_store,
                       air_store_id==id),
                median)
    return(as.numeric(v))
}

predict_by_median_by_store_dow <- function(id, date_){
    v <- select(filter(all_median_by_store_dow,
                       air_store_id==id,
                       day_of_week==weekdays(date_)),
                median)
    return(as.numeric(v))
}

parse_test_id <- function(data){
    if ( ! is.vector(data) ) {
        print("Error: data is not a vector")
        return(0)
    }
    ids <- unique(
             map_chr(
               map(data, strsplit, "_"),
               function(x) {
                 id <- paste(x[[1]][1], x[[1]][2], sep="_")
                 return(id)
               }))
    return(ids)
}
