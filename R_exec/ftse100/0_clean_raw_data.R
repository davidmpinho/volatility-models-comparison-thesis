path_global_variables <- here::here('/R_exec/ftse100/global_variables.R')
invisible(source(path_global_variables))
invisible(source(file=paste0(path_r_scripts, '1_generate_measures.R')))

# Aggregate and clean data ----------------------------------------------------------
data.table::setDTthreads(threads=max_cores)
data_intraday <- aggregate_data(path_data=path_raw_data, regex_pattern=regex_data_files)
clean_ftse_data(data_intraday=data_intraday, path_vftse=path_vftse_raw, 
                path_daily=path_daily_raw, path_save_intraday=path_intraday_clean,
                path_save_daily=path_daily_clean, path_bst=path_bst,
                path_save_vftse=path_vftse_clean, date_first=date_first,
                date_last=date_last, time_gmt_start=time_gmt_start,
                time_gmt_end=time_gmt_end, exclude_below_n_obs=max_obs)
data.table::setDTthreads(threads=1)
