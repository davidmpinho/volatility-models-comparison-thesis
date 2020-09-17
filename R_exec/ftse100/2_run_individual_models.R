library(foreach)
library(dplyr)
# Source files with functions and variables ------------------------------------
path_global_variables <- here::here('/R_exec/ftse100/global_variables.R')
invisible(source(path_global_variables))
script_names <- list.files(path=path_r_scripts, pattern="*.R$", full.names=TRUE)
invisible(lapply(script_names, FUN=source))

# Import data -----------------------------------------------------------------
invisible(source(path_import_vars))
data.table::setDTthreads(threads=6)  # Change this if you are running on another PC

# ARGUMENTS INDIVIDUAL MODELS -----------------------------------------------------------------
# ARMA options
arma_args_rv <- list(
    data_y = list(rv_ts),  
    path_folder_save = path_results_fit,
    vol_proxy = 'rv',
    n_ar = c(0, 1),
    n_d = c(0),
    n_ma = c(0, 1),
    method = 'ML',
    save_fit_in_pos = start_forecast)
arma_args_sqrret <- arma_args_rv
arma_args_sqrret$data_y <- list(squared_ret_ts)
arma_args_sqrret$vol_proxy <- 'squared_ret'
arma_args_rv <- get_parameter_combinations(list_parameters=arma_args_rv)
arma_args_rv <- clean_arima_models(table_models=arma_args_rv)
arma_args_sqrret <- get_parameter_combinations(list_parameters=arma_args_sqrret)
arma_args_sqrret <- clean_arima_models(table_models=arma_args_sqrret)
# GARCH options
garch_args <- get_parameter_combinations(list_parameters=list(
    model = c('GARCH', 'AVGARCH', 'GJRGARCH', 'TGARCH', 'NGARCH',
              'NAGARCH', 'APARCH', 'ALLGARCH', 'eGARCH', 'iGARCH',
              'csGARCH', 'realGARCH', 'ARCH'),
    lag_p = c(1), lag_q = c(1), arma_p = c(0), arma_q = c(0),
    include_cond_mean = FALSE, include_arch_m = FALSE,
    cond_dist = c('norm'), data_rv = list(rv_xts),
    data_returns = list(returns_xts), path_folder_save = path_results_fit,
    save_fit_in_pos = start_forecast))
garch_args <- clean_garch_models(all_models=garch_args)
# HAR options
har_models <- get_parameter_combinations(list_parameters=list(
    rv=list(c(1,5,22)), j=list(NA, c(1,5,22)), q=list(NA, c(1)),
    c=list(NA, c(1,5,22)), s=list(NA, c(1))))
har_models <- clean_har_models(models=har_models)
har_models_l <- split(har_models, seq(1, NROW(har_models)))
har_args <- as.data.frame(cbind(parameter_list=har_models_l, data=list(data_har_model),
                                save_fit_in_pos=start_forecast,
                                path_folder_save=path_results_fit,
                                rv_log=FALSE))
# Adding HAR-RV-log
har_log_par_list <- list(rv=c(1, 5, 22), j=NA, q=NA, c=NA, s=NA)
har_args <- dplyr::add_row(.data = har_args,
                           parameter_list = list(as.data.frame(t(as.matrix(har_log_par_list)))),  # Good reason to hate base R, right here.
                           data = list(dplyr::mutate(data_har_model, rv = log(rv))),
                           save_fit_in_pos = list(start_forecast),
                           path_folder_save = list(path_results_fit),
                           rv_log = list(TRUE))
not_rv_and_j <- unique(which(unlist(
    lapply(X=har_args$parameter_list,
           FUN= function (df) (is.na(df$rv) | is.na(df$j))))))
har_args <- har_args[not_rv_and_j, ]

do_par_options <- list(export = ls(rlang::global_env()),
                       packages = parallel_packages,
                       max_cores = max_cores)

# RUN INDIVIDUAL MODELS -----------------------------------------------------------------
# Random walk
rand_walk_forecast(data=realised_measures$rv_5, int_start_forecast=start_forecast,
                   path_folder_save=path_results, vol_proxy='rv5',
                   save_output=TRUE, return_output=FALSE)
rand_walk_forecast(data=squared_ret, int_start_forecast=start_forecast,
                   path_folder_save=path_results, vol_proxy='squared_ret',
                   save_output=TRUE, return_output=FALSE)
rand_walk_forecast(data=imp_vol, int_start_forecast=start_forecast,
                   path_folder_save=path_results, vol_proxy='iv',
                   save_output=TRUE, return_output=FALSE)
# Rolling mean and historical mean
for (w in window_values) {
    roll_avg_forecast(data=realised_measures$rv_5, window=w,
                      int_start_forecast=start_forecast,
                      path_folder_save=path_results, vol_proxy='rv5',
                      save_output=TRUE, return_output=FALSE)
    roll_avg_forecast(data=squared_ret, window=w,
                      int_start_forecast=start_forecast,
                      path_folder_save=path_results, vol_proxy='squared_ret',
                      save_output=TRUE, return_output=FALSE)
}
# Exponential smoothing
for (b in beta_values) {
    exp_smooth_forecast(data=realised_measures$rv_5, beta=b,
                        int_start_forecast=start_forecast,
                        path_folder_save=path_results, vol_proxy='rv5',
                        save_output=TRUE, return_output=FALSE)
    exp_smooth_forecast(data=squared_ret, beta=b,
                        int_start_forecast=start_forecast,
                        path_folder_save=path_results, vol_proxy='squared_ret',
                        save_output=TRUE, return_output=FALSE)
}

# ARMA models
roll_forecast(model_func=return_arima_forecast, model_func_args=arma_args_rv,
              pos_start_forecast=start_forecast, n_forecasts=n_forecasts, do_par=FALSE,
              do_par_options=do_par_options, path_folder_save=path_results,
              model_family='arima')
roll_forecast(model_func=return_arima_forecast, model_func_args=arma_args_sqrret,
              pos_start_forecast=start_forecast, n_forecasts=n_forecasts, do_par=FALSE,
              do_par_options=do_par_options, path_folder_save=path_results,
              model_family='arima')

# GARCH
roll_forecast(model_func=return_garch_forecast, model_func_args=garch_args,
              pos_start_forecast=start_forecast, n_forecasts=n_forecasts,
              do_par=TRUE, do_par_options=do_par_options,
              path_folder_save=path_results, model_family='garch')

# HAR
roll_forecast(model_func=return_har_forecast, model_func_args=har_args,
              pos_start_forecast=start_forecast, n_forecasts=n_forecasts,
              do_par=FALSE, do_par_options=do_par_options,
              path_folder_save=path_results, model_family='har')

