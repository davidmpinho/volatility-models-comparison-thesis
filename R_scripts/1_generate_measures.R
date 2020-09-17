
aggregate_data <- function (path_data, regex_pattern) {
    # Takes every file in the data path ('path_data') that matches a regex pattern
    # ('regex_pattern'). I am assuming that only csv files are going to be matched.
    # For more information about the schemas of those csv files, check the README.
    #
    # --------------------------------------------------------------------------
    # Input
    # path_data (type: character)
    #       The absolute ("/home/Desktop/") or relative ("./../Desktop")
    #       path of the data.
    #
    # regex_pattern (type: character) 
    #       Has to be a valid regular expression. Only the files that match
    #       that regular expression in the data_path specified will be aggregated.
    #
    # --------------------------------------------------------------------------
    # Output (type: data.table)
    #       Returns a data.table with columns 'date', 'time', and 'price'.
    #
    path_files <- list.files(path=path_data, pattern=regex_pattern, full.names=TRUE)
    data_agg <- lapply(path_files, data.table::fread)
    data_agg <- data.table::rbindlist(data_agg)
    colnames(data_agg) <- c('date', 'time', 'price')
    data_agg[ , date := as.Date(date, format='%d.%m.%Y')]
    data_agg[ , time := data.table::as.ITime(time)]
    return(data_agg)
}

clean_ftse_data <- function (data_intraday, path_bst, path_vftse, path_daily_ret,
                             path_save_intraday, path_save_vftse, path_save_daily,
                             date_first, date_last, time_gmt_start, time_gmt_end,
                             exclude_below_n_obs) {
    # All of the data cleaning decisions are analyzed in the file
    # 'data_cleaning_analysis.Rmd' situated in the 'R_notebooks' folder.
    # Read the README to know what each data and path variable stands for.
    #
    print("Started cleaning the data.")
    time_bst_start <- time_gmt_start - 60^2
    time_bst_end <- time_gmt_end - 60^2

    data_vftse <- data.table::fread(path_vftse, header=TRUE)
    data_vftse[ , date := as.Date(date, format='%d-%m-%Y')]

    data_daily <- data.table::fread(path_daily_ret, header=TRUE)
    data_daily[ , date := as.Date(date, format='%d-%m-%Y')]
    data_daily <- data_daily[ , price := ftse_100_price_index][ , c('date', 'price')]
    data_daily[ , price := gsub(pattern=',', replacement='.', price)]
    data_daily[ , price := as.numeric(price)]

    data_bst <- data.table::fread(path_bst, header=FALSE, stringsAsFactors=FALSE)
    colnames(data_bst) <- c('start', 'end')
    data_bst[ , start:=as.Date(start, format='%d-%m-%Y')]
    data_bst[ , end:=as.Date(end, format='%d-%m-%Y')]

    data_intraday[ , date := as.Date(date)]
    data_intraday[ , time := data.table::as.ITime(time)]

    # Eliminates all duplicated rows except the last one in the group of duplicates
    duplicated <- duplicated(data_intraday[ , c('date', 'time')], fromLast=TRUE)
    data_intraday[ , is_duplicated := duplicated]
    data_intraday[ , is_day_change := ((date - data.table::shift(date)) > 0)]
    data_intraday[ , is_unordered_day := ((date - data.table::shift(date)) < 0)]
    data_intraday[ , time_change := as.numeric(time - data.table::shift(time))]
    data_intraday[ , is_unordered_time := ((time_change < 0) & (!is_day_change))]
    bst_dates <- unlist(Map(`:`, data_bst$start, data_bst$end))
    data_intraday[ , is_bst := date %in% bst_dates]
    
    # Flag observations that are off-hours (8:00-16:30 GMT or 7:00-15:30 BST)
    time_agg <- data_intraday$time
    is_gmt <- !(data_intraday$is_bst)
    is_bst <- !is_gmt
    off_hours_gmt <- (((time_agg < time_gmt_start) | (time_agg > time_gmt_end)) & is_gmt)
    off_hours_bst <- (((time_agg < time_bst_start) | (time_agg > time_bst_end)) & is_bst)
    data_intraday[ , is_off_hours := (off_hours_bst | off_hours_gmt)]
                                  
    # Delete intraday observations
    data_intraday <- data_intraday[(!is_duplicated)
                          & (!is_unordered_time)
                          & (!is_unordered_day)
                          & (!is_off_hours | (is_off_hours
                                                & (   (date == '2000-04-05')
                                                    | (date == '2010-03-29')
                                                    | (date == '2010-03-30')
                                                    | (date == '2010-03-31')
                                                  )
                                              )
                            )
                          & ((date != '2000-03-13') & (time != '16:22:45'))
                          & ((date > date_first ) & (date < date_last))]
    data_intraday <- data_intraday[ , c('date', 'time', 'price')]
    
    # Flag days with less than 60% of daily observations
    data_intraday[ , date_and_time := as.POSIXct(paste(date, time),
                                        format="%Y-%m-%d %H:%M:%S",
                                        tz="Europe/London")] 
    data_intraday[ , min_1_ceiling := lubridate::ceiling_date(date_and_time, unit='1 min')]
    
    min_1_data <- data_intraday[ , data.table::last(min_1_ceiling), by=min_1_ceiling]
    n_obs <- min_1_data[ , .N, by=as.Date(min_1_ceiling)]
    colnames(n_obs) <- c('date', 'obs')
    short_days <- n_obs[(n_obs$obs < exclude_below_n_obs)]$date  # 306 is 60% of 510 observations
    
    # Delete the days that aren't needed, save the file, and return it
    data_intraday <- data_intraday[!(date %in% short_days)]
    
    # Eliminate dates from the vftse that aren't in data_intraday
    dates_vftse <- data_vftse$date
    dates_intraday <- unique(data_intraday$date)
    dates_vftse <- dates_vftse[(dates_vftse %in% dates_intraday)]
    
    # Eliminate dates from data_clean that aren't in the vftse
    data_intraday <- data_intraday[(date %in% dates_vftse)]
    data_vftse <- data_vftse[(date %in% dates_vftse)]
    data_daily <- data_daily[(date %in% data_intraday$date)]
    
    data_intraday <- data_intraday[ , c('date', 'time', 'price')]
    data.table::fwrite(data_intraday, path_save_intraday)
    data.table::fwrite(data_vftse, path_save_vftse)
    data.table::fwrite(data_daily, path_save_daily)
    print("The cleaned data was saved.")
}

get_realised_measures <- function(path_data_clean, path_realised_measures, minutes) {
    # Generates and saves the realized measures. Each measure is saved only in one file.
    # Everything is calculated using intra-daily log returns.
    # Note: realized measures only take into account the intra-daily returns.
    data_clean <- data.table::fread(path_data_clean, header=TRUE)
    data_clean[ , date := as.Date(date)]
    data_clean[ , time := data.table::as.ITime(time)]
    data_clean[ , date_and_time := as.POSIXct(paste(date, time),
                                              format="%Y-%m-%d %H:%M:%S",
                                              tz="Europe/London")]
    realised_measures <- data.frame(date=unique(data_clean$date))
    for (m in minutes){
        print(paste("Calculating", m, "minute realised measures."))
        data_clean <- .add_timestamp_ceiling(data=data_clean, min=m)
        data_k_min <- .get_k_min_data(data=data_clean)
        
        realised_measures[ , paste('rv', m, sep='_')] <- .calc_rv(data_k_min=data_k_min)
        realised_measures[ , paste('rq', m, sep='_')] <- .calc_rq(data_k_min=data_k_min)
        realised_measures[ , paste('bpv', m, sep='_')] <- .calc_bpv(data_k_min=data_k_min)
        realised_measures[ , paste('rsv', m, sep='_')] <- .calc_rsv(data_k_min=data_k_min)
        realised_measures[ , paste('trv', m, sep='_')] <- .calc_trv(data_k_min=data_k_min)
    }
    data.table::fwrite(realised_measures, path_realised_measures)
    print("Calculated and saved all the realised measures.")
}
.add_timestamp_ceiling <- function(data, min) {
    # Add minutely timestamp ceiling.
    data[ , k_min_ceiling := lubridate::ceiling_date(date_and_time, paste(min, 'min'))]
    data[ , is_day_change := ((date - data.table::shift(date)) > 0)]
    data[ , is_day_change := c(TRUE, is_day_change[2:length(is_day_change)])]
    last_k_min <- data[ , data.table::last(date_and_time), by=k_min_ceiling]$V1
    data[ , flagged_prices := ((date_and_time %in% last_k_min) | is_day_change) ]
    return(data)
}
.get_k_min_data <- function (data) {
    # Convert minutely data to k-minute data.
    data_k_min <- data[(flagged_prices)]
    data_k_min[ , intra_ret := log(price / data.table::shift(price)), by=date]
    data_k_min <- na.omit(data_k_min)
    return(data_k_min)
}
.calc_rv <- function (data_k_min) {
    # Calculate realised variance.
    rv <- data_k_min[ , sum(intra_ret^2), by=date]$V1
    return(rv)
}
.calc_rq <- function (data_k_min) {
    # Calculate realised quarticity.
    rq <- data_k_min[ , (length(intra_ret)/3 * sum(intra_ret^4)), by=date]$V1
    return(rq)
}
.calc_bpv <- function (data_k_min) {
    # Calculate bipower variation.
    mu_1 <- sqrt(2/base::pi)
    bpv <- data_k_min[ , (mu_1^-2 * sum(abs(intra_ret[2:length(intra_ret)])
                                            * data.table::shift(abs(intra_ret))[2:length(intra_ret)])),
                         by=date]$V1
    return(bpv)
}
.calc_rsv <- function (data_k_min) {
    # Calculate realised semi-variance.
    data_k_min[ , is_positive := ifelse(intra_ret >= 0, yes=1, no=0)]
    rsv <- data_k_min[ , sum((intra_ret * is_positive)^2), by=date]$V1
    return(rsv)
}
.calc_trv <- function (data_k_min) {
    # Calculate truncated realised variance.
    data_k_min[ , mean_rv_day := mean(intra_ret^2), by=date]
    trv <- data_k_min[which(intra_ret^2 > (3*mean_rv_day)), sum(intra_ret^2),
                      by=date]$V1
    return(trv)
}

get_returns <- function(path_daily, path_save_ret) {
    # Generates absolute and squared returns.
    data <- data.table::fread(path_daily, header=TRUE)
    data[ , log_ret := log(price / data.table::shift(price))]
    data[ , squared_ret := log_ret^2]
    data[ , abs_ret := abs(log_ret)]
    data.table::fwrite(data, path_save_ret)
}
