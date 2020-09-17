
# Paths high-level folders ------------------------------------------------------
path_r_scripts <- here::here('R_scripts//')
path_data_ftse <- here::here('data/ftse100//')

# Path to import variables --------------------------------------------------------
path_import_vars <- here::here('R_exec/ftse100/import_volatility_measures.R')

# Paths data folders ------------------------------------------------------------
path_raw_data <- paste0(path_data_ftse, 'raw_data/')
path_clean_data <- paste0(path_data_ftse, 'clean_data/')
path_measures <- paste0(path_data_ftse, 'measures/')
path_results <- paste0(path_data_ftse, 'ind_results/')
path_results_fit <- paste0(path_data_ftse, 'ind_results_fit/')
path_comb_results <- paste0(path_data_ftse, 'comb_results/')
path_comb_results_fit <- paste0(path_data_ftse, 'comb_results_fit/')
path_comb_weights_matrix <- paste0(path_data_ftse, 'comb_weights_matrix/')
path_comb_reg_predictions <- paste0(path_data_ftse, 'comb_reg_predictions/')
path_comb_reg_all <- paste0(path_data_ftse, 'comb_reg_all/')
path_mcs <- paste0(path_data_ftse, 'mcs/')

# Paths data files --------------------------------------------------------------
path_bst <- paste0(path_raw_data, 'bst.csv')
path_vftse_raw <- paste0(path_raw_data, 'vftse.csv')
path_vftse_clean <- paste0(path_clean_data, 'vtse_cleaned.csv')
path_daily_raw <- paste0(path_raw_data, 'price_ftse100_datastream.csv')
path_daily_clean <- paste0(path_clean_data, 'daily_ret_clean.csv')
path_intraday_clean <- paste0(path_clean_data, 'intraday_ret_clean.csv')
path_returns <- paste0(path_measures, 'returns.csv')
path_realised_measures <- paste0(path_measures, 'realised_measures.csv')

# Model options -------------------------------------------------------------------
realised_measures_mins <- 5 #get_int_no_remainder(start=1, end=60, numerator=510)
beta_values <- c(0.05, seq(0.9, 0.1, length.out = 9), 0.95)  # For exponential smoothing
window_values <- c(5, 21, 63, 126, 252, 504, 1008, 2734) 
mcs_alpha <- 0.2

# Data cleaning options -----------------------------------------------------------
date_first <- as.Date('2000-01-01')
date_last <- as.Date('2010-12-31')  
time_gmt_start <- data.table::as.ITime('08:00:00')
time_gmt_end <- data.table::as.ITime('16:30:00')
max_obs <- 306

# Data columns --------------------------------------------------------------------
col_mins <- '5'  # Can only be one (character) integer
sqr_ret_col <- 'squared_ret'
ret_col <- 'log_ret'

# Parallel options -----------------------------------------------------------------
max_cores <- 6
parallel_packages <- c('dplyr', 'foreach')

# Train/test set -------------------------------------------------------------------
# NOTE: it should START in 1232 from the beginning of RV.
start_forecast <- 1232
# NOTE: the cross-validation STARTS at 1482 (+250)
start_comb_forecast_cv <- 250
# NOTE: all OOS forecasts START at 1731 (+249) -- total of 1004 obs
start_comb_forecast <- 249
start_oos_forecast <- start_forecast+start_comb_forecast_cv+start_comb_forecast
# save_fit_in_pos <- c(1231, 2733)  # If you want to save the objects after the first and last forecast 
n_forecasts <- 2734 - start_forecast + 1
n_forecasts_cv_comb <- n_forecasts - start_comb_forecast_cv  
n_forecasts_comb <- n_forecasts_cv_comb - start_comb_forecast 

# Regulgar expressions (REGEX) --------------------------------------------------------
regex_data_files <- '20[0-9][0-9].txt'

