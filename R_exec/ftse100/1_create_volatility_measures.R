path_global_variables <- here::here('R_exec/ftse100/global_variables.R')
invisible(source(path_global_variables))
invisible(source(file=paste0(path_r_scripts, '1_generate_measures.R')))

# Create volatility measures ----------------------------------------------------------
get_realised_measures(path_data_clean=path_intraday_clean,
                      path_realised_measures=path_realised_measures,
                      minutes=realised_measures_mins)
get_returns(path_daily=path_daily_clean, path_save_ret=path_returns)

